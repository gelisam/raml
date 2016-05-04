{-# LANGUAGE OverloadedStrings #-}
module Raml.Classifier where

import Control.Applicative
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Yaml (ToJSON(..))

import Data.Yaml.Extra
import Raml.Common
import           Raml.Normalizer (NormalizedTree(..))
import qualified Raml.Normalizer as Normalizer


data ObjectType
  = Object
  | ObjectRef TypeName
  deriving (Show, Eq)

data StringType
  = String
  | StringRef TypeName
  deriving (Show, Eq)

data UnionBranch
  = ObjectBranch ObjectType
  | StringBranch StringType
  deriving (Show, Eq)
type UnionType = [UnionBranch]

data Type
  = ObjectType ObjectType
  | StringType StringType
  | UnionType UnionType
  deriving (Show, Eq)


data ObjectProps = ObjectProps
  { parentObjectType :: ObjectType
  , properties :: Map PropertyName TypeProps
  , objectDiscriminator :: Maybe PropertyName
  } deriving (Show, Eq)

data StringProps = StringProps
  { parentStringType :: StringType
  , stringPattern :: Maybe Regexp
  } deriving (Show, Eq)

type UnionProps = UnionType

data TypeProps
  = ObjectTypeProps ObjectProps
  | StringTypeProps StringProps
  | UnionTypeProps UnionProps
  deriving (Show, Eq)


type SymbolTable = Map TypeName TypeProps

newtype ClassifiedTree = ClassifiedTree
  { unClassifiedTree :: Map TypeName TypeProps
  } deriving (Show, Eq)


instance ToJSON ObjectType where
  toJSON Object = YamlString "object"
  toJSON (ObjectRef x) = YamlString x

instance ToJSON StringType where
  toJSON String = YamlString "string"
  toJSON (StringRef x) = YamlString x

instance ToJSON UnionBranch where
  toJSON (ObjectBranch x) = toJSON x
  toJSON (StringBranch x) = toJSON x

instance ToJSON Type where
  toJSON (ObjectType Object) = YamlString "object"
  toJSON (ObjectType (ObjectRef x)) = YamlString (x ++ " :: object")
  toJSON (StringType String) = YamlString "string"
  toJSON (StringType (StringRef x)) = YamlString (x ++ " :: string")
  toJSON (UnionType xs) = toJSON xs

instance ToJSON ObjectProps where
  toJSON t = object [ "type" .=! parentObjectType t
                    , "properties" .=! properties t
                    , "discriminator" .=? objectDiscriminator t
                    ]

instance ToJSON StringProps where
  toJSON t = object [ "type" .=! parentStringType t
                    , "pattern" .=? stringPattern t
                    ]

instance ToJSON TypeProps where
  toJSON (ObjectTypeProps x) = toJSON x
  toJSON (StringTypeProps x) = toJSON x
  toJSON (UnionTypeProps x) = toJSON x

instance ToJSON ClassifiedTree where
  toJSON (ClassifiedTree x) = object [ "types" .=! x ]


classifyUnionBranch :: SymbolTable -> Normalizer.TypeExpr -> UnionBranch
classifyUnionBranch symbolTable = go
  where
    go :: Normalizer.TypeExpr -> UnionBranch
    go Normalizer.Object = ObjectBranch Object
    go Normalizer.String = StringBranch String
    go (Normalizer.Ref x) = case symbolTable ! x of
        ObjectTypeProps _ -> ObjectBranch (ObjectRef x)
        StringTypeProps _ -> StringBranch (StringRef x)
        UnionTypeProps xs -> error "unsupported: nested union type %s" (show xs)
    go (Normalizer.Union xs) = error "unsupported: nested union type %s" (show xs)

inheritFromTypeExpr :: SymbolTable -> Normalizer.TypeExpr -> TypeProps
inheritFromTypeExpr symbolTable = go
  where
    go :: Normalizer.TypeExpr -> TypeProps
    go Normalizer.Object = ObjectTypeProps
                         $ ObjectProps
                         { parentObjectType = Object
                         , properties = Map.empty
                         , objectDiscriminator = Nothing
                         }
    go Normalizer.String = StringTypeProps
                         $ StringProps
                         { parentStringType = String
                         , stringPattern = Nothing
                         }
    go (Normalizer.Ref x) = case symbolTable ! x of
        ObjectTypeProps p -> ObjectTypeProps $ p { parentObjectType = ObjectRef x }
        StringTypeProps p -> StringTypeProps $ p { parentStringType = StringRef x }
        UnionTypeProps p -> UnionTypeProps p
    go (Normalizer.Union xs) = UnionTypeProps
                             $ map (classifyUnionBranch symbolTable) xs


mergeProperties :: SymbolTable
                -> Map PropertyName TypeProps
                -> Maybe (Map PropertyName Normalizer.TypeProps)
                -> Map PropertyName TypeProps
mergeProperties symbolTable = go
  where
    go :: Map PropertyName TypeProps
       -> Maybe (Map PropertyName Normalizer.TypeProps)
       -> Map PropertyName TypeProps
    go x Nothing = x
    go x (Just y) | Map.null x =
        fmap (classifyTypeProps symbolTable) y
    go x (Just y) =
        error "unsupported: merging properties %s and %s" (show x) (show y)

mergeDiscriminators :: Maybe PropertyName
                    -> Maybe PropertyName
                    -> Maybe PropertyName
mergeDiscriminators (Just x) (Just y) =
    error "illegal: disriminator %s cannot replace %s" (show y) (show x)
mergeDiscriminators x y = x <|> y

mergeStringPatterns :: Maybe Regexp
                    -> Maybe Regexp
                    -> Maybe Regexp
mergeStringPatterns (Just x) (Just y) =
    error "unsupported: merging string patterns %s and %s" (show x) (show y)
mergeStringPatterns x y = x <|> y


mergeObjectProps :: SymbolTable
                 -> ObjectProps
                 -> Maybe (Map PropertyName Normalizer.TypeProps)
                 -> Maybe PropertyName
                 -> ObjectProps
mergeObjectProps symbolTable
                 (ObjectProps inheritedParent
                              inheritedProperties
                              inheritedDiscriminator)
                 newProperties
                 newDiscriminator =
    ObjectProps inheritedParent
                (mergeProperties symbolTable inheritedProperties newProperties)
                (mergeDiscriminators inheritedDiscriminator newDiscriminator)

mergeStringProps :: StringProps
                 -> Maybe Regexp
                 -> StringProps
mergeStringProps (StringProps inheritedParent
                              inheritedStringPattern)
                 newStringPattern =
    StringProps inheritedParent
                (mergeStringPatterns inheritedStringPattern newStringPattern)

mergeUnionProps :: UnionProps
                -> UnionProps
mergeUnionProps = id


mergeTypeProps :: SymbolTable
               -> TypeProps
               -> Maybe (Map PropertyName Normalizer.TypeProps)
               -> Maybe PropertyName
               -> Maybe Regexp
               -> TypeProps
mergeTypeProps symbolTable = go
  where
    go  :: TypeProps
        -> Maybe (Map PropertyName Normalizer.TypeProps)
        -> Maybe PropertyName
        -> Maybe Regexp
        -> TypeProps
    go (ObjectTypeProps objectProps) newProperties newDiscriminator Nothing =
        ObjectTypeProps (mergeObjectProps symbolTable
                                          objectProps
                                          newProperties
                                          newDiscriminator)
    go (ObjectTypeProps _) _ _ (Just newStringPattern) =
        error "illegal string pattern %s in object type" (show newStringPattern)
    go (StringTypeProps stringProps) Nothing Nothing newStringPattern =
        StringTypeProps (mergeStringProps stringProps newStringPattern)
    go (StringTypeProps _) (Just newProperties) _ _ =
        error "illegal properties %s in string type" (show newProperties)
    go (StringTypeProps _) _ (Just newDiscriminator) _ =
        error "illegal discriminator %s in string type" (show newDiscriminator)
    go (UnionTypeProps unionProps) Nothing Nothing Nothing =
        UnionTypeProps (mergeUnionProps unionProps)
    go (UnionTypeProps _) (Just newProperties) _ _ =
        error "unsupported: extra properties %s in union type" (show newProperties)
    go (UnionTypeProps _) _ (Just newDiscriminator) _ =
        error "unsupported: extra discriminator %s in union type" (show newDiscriminator)
    go (UnionTypeProps _) _ _ (Just newStringPattern) =
        error "illegal string pattern %s in union type" (show newStringPattern)


classifyTypeProps :: SymbolTable -> Normalizer.TypeProps -> TypeProps
classifyTypeProps symbolTable newTypeProps = mergedTypeProps
  where
    Normalizer.TypeProps parentType newProperties newDiscriminator newStringPattern
      = newTypeProps
    
    inheritedProps :: TypeProps
    inheritedProps = inheritFromTypeExpr symbolTable parentType
    
    mergedTypeProps :: TypeProps
    mergedTypeProps = mergeTypeProps symbolTable
                                     inheritedProps 
                                     newProperties
                                     newDiscriminator
                                     newStringPattern

-- |
-- >>> import Raml.Parser
-- >>> import Raml.Normalizer
-- >>> r <- classify <$> normalize <$> parse <$> readYaml "tests/sample.in"
-- >>> printAsYaml r
-- types:
--   BooleanType:
--     discriminator: constructor
--     type: Alternative
--     properties: {}
--   DateType:
--     discriminator: constructor
--     type: Alternative
--     properties:
--       dateFormat:
--         pattern: ! '[YMD]+[-\.][YMD]+[-\.\/][YMD]+'
--         type: string
--   Alternative:
--     discriminator: constructor
--     type: object
--     properties: {}
--   Field:
--     type: object
--     properties:
--       name:
--         type: string
--       dataType:
--       - StringType
--       - NumberType
--       - DateType
--       - BooleanType
--   NumberType:
--     discriminator: constructor
--     type: Alternative
--     properties: {}
--   StringType:
--     discriminator: constructor
--     type: Alternative
--     properties: {}
--   DataType:
--   - StringType
--   - NumberType
--   - DateType
--   - BooleanType
classify :: NormalizedTree -> ClassifiedTree
classify = ClassifiedTree . go . unNormalizedTree
  where
    go :: Normalizer.SymbolTable -> SymbolTable
    go oldSymbolTable = newSymbolTable
      where
        newSymbolTable :: SymbolTable
        newSymbolTable = fmap (classifyTypeProps newSymbolTable) oldSymbolTable 
