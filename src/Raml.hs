{-# LANGUAGE ScopedTypeVariables, TupleSections, ViewPatterns #-}
module Raml where

import Control.Applicative
import Control.Monad
import Data.List
import           Data.Yaml (Value)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map, (!))
import Text.Printf

import qualified Raml.Syntax as Syntax
import           Raml.Syntax (PrimitiveType(..))

-- $setup
-- >>> import qualified Data.Yaml as Yaml


newtype UnitName   = UnitName   { unUnitName   :: String } deriving (Show, Ord, Eq)
newtype ObjectName = ObjectName { unObjectName :: String } deriving (Show, Ord, Eq)
newtype StringName = StringName { unStringName :: String } deriving (Show, Ord, Eq)
newtype UnionName  = UnionName  { unUnionName  :: String } deriving (Show, Ord, Eq)

data Raml = Raml
  { unitTypes   :: Map UnitName   UnitDesc
  , objectTypes :: Map ObjectName ObjectDesc
  , stringTypes :: Map StringName StringDesc
  , unionTypes  :: Map UnionName  UnionDesc
  } deriving (Show, Eq)

data Type
  = PrimitiveType PrimitiveType
  | UnitType   UnitName
  | ObjectType ObjectName
  | StringType StringName
  | UnionType  UnionName
  | MaybeType  Type
  deriving (Show, Eq)

data UnitDesc = UnitDesc
  { unitDiscriminator :: String
  } deriving (Show, Eq)

data ObjectDesc = ObjectDesc
  { parentType :: Maybe ObjectName
  , properties :: Map String PropertyDesc
  , objectDiscriminator  :: Maybe String
  } deriving (Show, Eq)

data PropertyDesc = PropertyDesc
  { type_ :: Type
  , default_ :: Maybe Value
  } deriving (Show, Eq)

data StringDesc = StringDesc
  { stringPattern :: Maybe String
  } deriving (Show, Eq)

data UnionDesc = UnionDesc
  { alternatives :: [Either UnitName ObjectName]
  } deriving (Show, Eq)


conflicting :: Show a => String -> a -> a -> Either String b
conflicting name x1 x2 = fail $ printf "conflicting %s: %s" name (show [x1, x2])

mergeConflicting :: Show a => String -> Maybe a -> Maybe a -> Either String (Maybe a)
mergeConflicting name (Just x1) (Just x2) = conflicting name x1 x2
mergeConflicting _ m1 m2 = return (m1 <|> m2)

unionConflicting :: (Show a, Ord k)
                 => String
                 -> Map k a
                 -> Map k a
                 -> Either String (Map k a)
unionConflicting name m1 m2 = sequenceA
                            $ Map.mergeWithKey (\_ x1 x2 -> Just $ conflicting name x1 x2)
                                               (fmap return)
                                               (fmap return)
                                               m1 m2


distributeMaps :: forall k a. Ord k => [a -> Bool] -> Map k a -> [Map k a]
distributeMaps predicates = map Map.fromList
                          . go predicates
                          . Map.toList
  where
    go :: [a -> Bool] -> [(k, a)] -> [[(k, a)]]
    go []     xs = [xs]
    go (p:ps) xs = xs1 : go ps xss
      where
        (xs1,xss) = partition (p . snd) xs

strengthen :: Ord k
           => (String -> k)
           -> (a -> Either String b)
           -> Map String a
           -> Either String (Map k b)
strengthen mapKey mapValue = fmap Map.fromList
                           . mapM go
                           . Map.toList
  where
    go (k,v) = case mapValue v of
      Left err -> Left (printf "%s: %s" k err)
      Right v' -> Right (mapKey k, v')

-- |
-- >>> validate <$> Data.Maybe.fromJust <$> Data.Yaml.decodeFile "tests/sample.in"
validate :: Syntax.Raml -> Either String Raml
validate (Syntax.Raml types) = do
    when (not (null unknownTypeDescs)) $ do
      let names = map fst $ Map.toList unknownTypeDescs
      fail $ printf "unrepresentable types: %s" (show names)
    Raml <$> unitTypes'
         <*> objectTypes'
         <*> stringTypes'
         <*> unionTypes'
  where
    inheritDiscriminator :: Syntax.TypeDesc -> Either String (Maybe String)
    inheritDiscriminator (Syntax.TypeDesc (Syntax.NamedType typeName) _ d _ _) = do
        parentDiscriminator <- inheritDiscriminator (types ! typeName)
        case (parentDiscriminator, d) of
          (Nothing, d') -> return d'
          (Just d', Nothing) -> return (Just d')
          (Just d1, Just d2) -> fail $ printf "conflicting discriminators: %s" (show [d1, d2])
    inheritDiscriminator (Syntax.discriminator -> Just d) = return $ Just d
    inheritDiscriminator _ = return $ Nothing
    
    inheritPattern :: Syntax.TypeDesc -> Either String (Maybe String)
    inheritPattern (Syntax.TypeDesc (Syntax.NamedType typeName) _ d _ _) = do
        parentPattern <- inheritPattern (types ! typeName)
        case (parentPattern, d) of
          (Nothing, d') -> return d'
          (Just d', Nothing) -> return (Just d')
          (Just d1, Just d2) -> fail $ printf "conflicting patterns: %s" (show [d1, d2])
    inheritPattern (Syntax.stringPattern -> Just d) = return $ Just d
    inheritPattern _ = return $ Nothing
    
    isUnit :: Syntax.TypeDesc -> Bool
    isUnit t = isObject t && Map.null (Syntax.properties t)
    
    isObject :: Syntax.TypeDesc -> Bool
    isObject (Syntax.parentType -> Syntax.PrimitiveType PrimitiveObject) = True
    isObject (Syntax.parentType -> Syntax.NamedType typeName) = isObject (types ! typeName)
    isObject _ = False
    
    isString :: Syntax.TypeDesc -> Bool
    isString (Syntax.parentType -> Syntax.PrimitiveType PrimitiveString) = True
    isString (Syntax.parentType -> Syntax.NamedType typeName) = isString (types ! typeName)
    isString _ = False
    
    isUnion :: Syntax.TypeDesc -> Bool
    isUnion (Syntax.parentType -> Syntax.Union _) = True
    isUnion _ = False
    
    [unitTypeDescs, objectTypeDescs, stringTypeDescs, unionTypeDescs, unknownTypeDescs] =
        distributeMaps [isUnit, isObject, isString, isUnion] types
    
    unitTypes'   = strengthen UnitName   unitDesc   unitTypeDescs
    objectTypes' = strengthen ObjectName objectDesc objectTypeDescs
    stringTypes' = strengthen StringName stringDesc stringTypeDescs
    unionTypes'  = strengthen UnionName  unionDesc  unionTypeDescs
    
    unitDesc :: Syntax.TypeDesc -> Either String UnitDesc
    unitDesc (Syntax.stringPattern -> Just _) =
        fail "a unit type shouldn't have a pattern"
    unitDesc (null . Syntax.enum -> False) =
        fail "a unit type shouldn't have enum values"
    unitDesc (null . Syntax.properties -> False) =
        fail "a unit type shouldn't have properties"
    unitDesc t = do
        d <- inheritDiscriminator t
        case d of
          Just d' -> return $ UnitDesc d'
          Nothing -> fail "a unit type should have a discriminator"
    
    objectDesc :: Syntax.TypeDesc -> Either String ObjectDesc
    objectDesc (Syntax.stringPattern -> Just _) =
        fail "an object type shouldn't have a pattern"
    objectDesc (null . Syntax.enum -> False) =
        fail "an object type shouldn't have enum values"
    objectDesc t@(Syntax.TypeDesc (Syntax.NamedType typeName) _ _ _ _) =
        ObjectDesc <$> pure (Just (ObjectName typeName))
                   <*> traverse propertyDesc (Syntax.properties t)
                   <*> inheritDiscriminator t
    objectDesc t@(Syntax.TypeDesc (Syntax.PrimitiveType PrimitiveObject) _ _ _ _) =
        ObjectDesc <$> pure Nothing
                   <*> traverse propertyDesc (Syntax.properties t)
                   <*> inheritDiscriminator t
    objectDesc (Syntax.TypeDesc (Syntax.PrimitiveType PrimitiveString) _ _ _ _) =
        fail "an object type shouldn't inherit from string"
    objectDesc (Syntax.TypeDesc (Syntax.Union _) _ _ _ _) =
        fail "an object type shouldn't inherit from union"
    
    propertyDesc :: Syntax.PropertyDesc -> Either String PropertyDesc
    propertyDesc p = PropertyDesc
                 <$> (possiblyOptional <$> expectNamedType (Syntax.type_ p))
                 <*> pure (Syntax.default_ p)
      where
        possiblyOptional :: Type -> Type
        possiblyOptional | Syntax.required p = MaybeType
                         | otherwise         = id
        
        expectNamedType :: Syntax.TypeExpr -> Either String Type
        expectNamedType (Syntax.PrimitiveType t) = return $ PrimitiveType t
        expectNamedType (Syntax.NamedType t) = return $ strengthenTypeName t
        expectNamedType (Syntax.Union _) =
          fail "a property's type shouldn't be a union"
        
        strengthenTypeName :: String -> Type
        strengthenTypeName s | isUnit   t = UnitType   (UnitName   s)
                             | isObject t = ObjectType (ObjectName s)
                             | isString t = StringType (StringName s)
                             | isUnion  t = UnionType  (UnionName  s)
                             | otherwise  = error "unknown type" -- should have been caught earlier
          where
            t = types ! s
    
    stringDesc :: Syntax.TypeDesc -> Either String StringDesc
    stringDesc t = StringDesc
               <$> inheritPattern t
    
    unionDesc :: Syntax.TypeDesc -> Either String UnionDesc
    unionDesc (Syntax.stringPattern -> Just _) =
        fail "a union type shouldn't have a pattern"
    unionDesc (null . Syntax.enum -> False) =
        fail "a union type shouldn't have enum values"
    unionDesc (null . Syntax.discriminator -> False) =
        fail "a union type shouldn't have a discriminator"
    unionDesc (null . Syntax.properties -> False) =
        fail "a union type shouldn't have properties"
    unionDesc (Syntax.TypeDesc (Syntax.PrimitiveType _) _ _ _ _) =
        fail "a union type shouldn't have a primitive type"
    unionDesc (Syntax.TypeDesc (Syntax.NamedType _) _ _ _ _) =
        fail "a union type shouldn't have a named parent"
    unionDesc (Syntax.TypeDesc (Syntax.Union ts) _ _ _ _) =
        UnionDesc <$> traverse expectNamedType ts
      where
        expectNamedType :: Syntax.TypeExpr -> Either String (Either UnitName ObjectName)
        expectNamedType (Syntax.PrimitiveType PrimitiveObject) =
          fail "unions shouldn't include \"object\""
        expectNamedType (Syntax.PrimitiveType PrimitiveString) =
          fail "unions shouldn't include \"string\""
        expectNamedType (Syntax.NamedType t) = strengthenTypeName t
        expectNamedType (Syntax.Union _) =
          fail "unions shouldn't include other unions"
        
        strengthenTypeName :: String -> Either String (Either UnitName ObjectName)
        strengthenTypeName s | isUnit   t = return $ Left  $ UnitName s
                             | isObject t = return $ Right $ ObjectName s
                             | isString t = fail "unions shouldn't include string types"
                             | isUnion  t = fail "unions shouldn't include other union types"
                             | otherwise  = error "unknown type" -- should have been caught earlier
          where
            t = types ! s
