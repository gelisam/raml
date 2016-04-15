{-# LANGUAGE OverloadedLists, RecordWildCards, ScopedTypeVariables, TupleSections, ViewPatterns #-}
module Raml.RamlB where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Functor.Identity
import           Data.Yaml (Value)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import Text.Printf

import Raml.RamlA
import qualified Raml.Syntax as Syntax
import           Raml.Syntax (PrimitiveType(..))

-- $setup
-- >>> import qualified Data.Yaml as Yaml


data Type
  = NamedType String Type
  | MaybeType Type
  | PrimitiveType PrimitiveType
  | UnitType UnitDesc
  | ObjectType ObjectDesc
  | StringType StringDesc
  | UnionType [Type]
  deriving (Show, Eq)

data UnitDesc = UnitDesc
  { unitDiscriminator :: String
  } deriving (Show, Eq)

data ObjectDesc = ObjectDesc
  { parentType :: Type
  , properties :: Map String PropertyDesc
  , objectDiscriminator :: Maybe String
  } deriving (Show, Eq)

data PropertyDesc = PropertyDesc
  { type_ :: Type
  , default_ :: Maybe Value
  } deriving (Show, Eq)

data StringDesc = StringDesc
  { stringPattern :: Maybe String
  } deriving (Show, Eq)

data UnionDesc = UnionDesc
  { alternatives :: [Type]
  } deriving (Show, Eq)


type RamlBT m = RamlAT (EitherT String m)
type RamlB = RamlBT Identity

runRamlBT :: MonadIO m => FilePath -> RamlBT m a -> m a
runRamlBT filePath ramlBT = do
    r <- runEitherT $ runRamlAT filePath ramlBT
    case r of
      Left err -> fail (show err)
      Right x -> return x


typesB :: Monad m => RamlBT m (Map String Type)
typesB = do
    r <- typesA
    traverse typeDescB r

-- |
-- >>> runRamlBT "tests/sample.in" namedTypesB
-- ["Alternative","BooleanType","DataType","DateType","Field","NumberType","StringType"]
namedTypesB :: Monad m => RamlBT m [String]
namedTypesB = namedTypesA

-- |
-- >>> runRamlBT "tests/sample.in" (lookupTypeB "BooleanType")
-- NamedType "BooleanType" (UnitType (UnitDesc {unitDiscriminator = "constructor"}))
lookupTypeB :: Monad m => String -> RamlBT m Type
lookupTypeB s = do
    typeDesc <- lookupTypeA s
    NamedType s <$> typeDescB typeDesc


conflictingB :: forall a b m. (Show a, Monad m) => String -> a -> a -> RamlBT m b
conflictingB name x1 x2 = fail $ printf "conflicting %s: %s" name (show ([x1, x2] :: [a]))

mergeConflictingB :: (Show a, Monad m) => String -> Maybe a -> Maybe a -> RamlBT m (Maybe a)
mergeConflictingB name (Just x1) (Just x2) = conflictingB name x1 x2
mergeConflictingB _ m1 m2 = return (m1 <|> m2)

unionConflictingB :: (Show a, Monad m)
                  => String
                  -> Map String a
                  -> Map String a
                  -> RamlBT m (Map String a)
unionConflictingB name m1 m2 = sequenceA
                             $ Map.mergeWithKey (\k x1 x2 -> Just $ conflictingB (printf "%s %s" name k) x1 x2)
                                                (fmap return)
                                                (fmap return)
                                                m1 m2


extendParentTypeB :: Monad m => Type -> Syntax.TypeDesc -> RamlBT m Type
extendParentTypeB (MaybeType _) _ = fail "Maybe is not a valid parent type"
extendParentTypeB p@(PrimitiveType PrimitiveObject) typeDesc =
    let objectDesc = ObjectDesc (PrimitiveType PrimitiveObject) [] Nothing
    in  extendFakeParentTypeB (ObjectType objectDesc) p typeDesc
extendParentTypeB p@(PrimitiveType PrimitiveString) typeDesc =
    let stringDesc = StringDesc Nothing
    in  extendFakeParentTypeB (StringType stringDesc) p typeDesc
extendParentTypeB p@(NamedType _ t) typeDesc =
    extendFakeParentTypeB t p typeDesc
extendParentTypeB p@(UnitType (UnitDesc d)) typeDesc =
    let objectDesc = ObjectDesc (PrimitiveType PrimitiveObject) [] (Just d)
    in  extendFakeParentTypeB (ObjectType objectDesc) p typeDesc
extendParentTypeB p@(ObjectType (ObjectDesc _ ps d)) (Syntax.TypeDesc _ ps' d' [] Nothing) = do
    ps'' <- traverse propertyDescB ps'
    ObjectType <$> (ObjectDesc <$> pure p
                               <*> unionConflictingB "property" ps ps''
                               <*> mergeConflictingB "discriminator" d d')
extendParentTypeB (ObjectType _) (Syntax.TypeDesc _ _ _ enum' Nothing) =
    fail $ printf "enum %s incompatible with object type" (show enum')
extendParentTypeB (ObjectType _) (Syntax.TypeDesc _ _ _ _ (Just p)) =
    fail $ printf "string pattern %s incompatible with object type" (show p)
extendParentTypeB (StringType (StringDesc p)) (Syntax.TypeDesc _ [] Nothing [] p') =
    StringType <$> (StringDesc <$> mergeConflictingB "string pattern" p p')
extendParentTypeB (StringType (StringDesc _)) (Syntax.TypeDesc _ [] Nothing enum' _) =
    fail $ printf "enum %s is valid but unimplemented" (show enum')
extendParentTypeB (StringType (StringDesc _)) (Syntax.TypeDesc _ [] (Just d) _ _) =
    fail $ printf "discriminator %s incompatible with string type" (show d)
extendParentTypeB (StringType (StringDesc _)) (Syntax.TypeDesc _ ps _ _ _) =
    fail $ printf "properties %s incompatible with string type" (show ps)
extendParentTypeB (UnionType ts) (Syntax.TypeDesc _ [] Nothing [] Nothing) =
    return $ UnionType ts
extendParentTypeB (UnionType _) (Syntax.TypeDesc _ [] Nothing [] (Just p)) =
    fail $ printf "string pattern %s incompatible with union type" (show p)
extendParentTypeB (UnionType _) (Syntax.TypeDesc _ [] Nothing enum' _) =
    fail $ printf "enum %s incompatible with union type" (show enum')
extendParentTypeB (UnionType _) (Syntax.TypeDesc _ [] (Just d) _ _) =
    fail $ printf "discriminator %s incompatible with union type" (show d)
extendParentTypeB (UnionType _) (Syntax.TypeDesc _ ps _ _ _) =
    fail $ printf "properties %s incompatible with union type" (show ps)

extendFakeParentTypeB :: Monad m => Type -> Type -> Syntax.TypeDesc -> RamlBT m Type
extendFakeParentTypeB fakeParent realParent typeDesc = do
    r <- extendParentTypeB fakeParent typeDesc
    case r of
      ObjectType o -> return $ ObjectType $ o { parentType = realParent }
      _ -> return r


typeDescB :: Monad m => Syntax.TypeDesc -> RamlBT m Type
typeDescB typeDesc = do
    parentType <- typeExprB (Syntax.parentType typeDesc)
    r <- extendParentTypeB parentType typeDesc
    case r of
      ObjectType (ObjectDesc _ [] (Just d)) -> return $ UnitType (UnitDesc d)
      _ -> return r

typeExprB :: Monad m => Syntax.TypeExpr -> RamlBT m Type
typeExprB (Syntax.PrimitiveType t) = return $ PrimitiveType t
typeExprB (Syntax.NamedType s) = lookupTypeB s
typeExprB (Syntax.Union ts) = UnionType <$> traverse typeExprB ts

propertyDescB :: Monad m => Syntax.PropertyDesc -> RamlBT m PropertyDesc
propertyDescB (Syntax.PropertyDesc False t d) = do
    PropertyDesc t' d' <- propertyDescB (Syntax.PropertyDesc False t d)
    return $ PropertyDesc (MaybeType t') d'
propertyDescB (Syntax.PropertyDesc True t d) =
    PropertyDesc <$> typeExprB t
                 <*> pure d
