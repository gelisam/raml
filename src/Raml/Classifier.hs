{-# LANGUAGE OverloadedStrings #-}
module Raml.Classifier where

import Data.Maybe
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Yaml.Ordered (ToJSON(..))
import Text.Printf

import Data.Yaml.Ordered.MyExtra
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

newtype UnionType
  = UnionRef TypeName
  deriving (Show, Eq)

data Type
  = ObjectType ObjectType
  | StringType StringType
  | UnionType UnionType
  deriving (Show, Eq)


data FieldProps
  = RegularField TypeName
  | CustomField TypeProps
  deriving (Show, Eq)

data UnionBranch
  = ObjectBranch ObjectType
  | StringBranch StringType
  deriving (Show, Eq)


data ObjectProps = ObjectProps
  { parentObjectType :: ObjectType
  , properties :: Map PropertyName FieldProps
  , objectDiscriminator :: Maybe Discriminator
  } deriving (Show, Eq)

data StringProps = StringProps
  { parentStringType :: StringType
  , stringPattern :: Maybe Regexp
  } deriving (Show, Eq)

newtype UnionProps = UnionProps
  { unionBranches :: [UnionBranch]
  } deriving (Show, Eq)

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

instance ToJSON UnionType where
  toJSON (UnionRef x) = YamlString x

instance ToJSON UnionBranch where
  toJSON (ObjectBranch x) = toJSON x
  toJSON (StringBranch x) = toJSON x

instance ToJSON Type where
  toJSON (ObjectType Object) = YamlString "object"
  toJSON (ObjectType (ObjectRef x)) = YamlString (x ++ " :: object")
  toJSON (StringType String) = YamlString "string"
  toJSON (StringType (StringRef x)) = YamlString (x ++ " :: string")
  toJSON (UnionType xs) = toJSON xs

instance ToJSON FieldProps where
  toJSON (RegularField typeName) = toJSON typeName
  toJSON (CustomField typeProps) = toJSON typeProps

instance ToJSON ObjectProps where
  toJSON t = object [ "type" .=! parentObjectType t
                    , "properties" .=! properties t
                    , "discriminator" .=? objectDiscriminator t
                    ]

instance ToJSON StringProps where
  toJSON t = object [ "type" .=! parentStringType t
                    , "pattern" .=? stringPattern t
                    ]

instance ToJSON UnionProps where
  toJSON (UnionProps branches) = toJSON branches

instance ToJSON TypeProps where
  toJSON (ObjectTypeProps x) = toJSON x
  toJSON (StringTypeProps x) = toJSON x
  toJSON (UnionTypeProps x) = toJSON x

instance ToJSON ClassifiedTree where
  toJSON (ClassifiedTree x) = object [ "types" .=! x ]


classifyProperties :: SymbolTable
                   -> Maybe (Map PropertyName Normalizer.TypeProps)
                   -> Map PropertyName FieldProps
classifyProperties symbolTable = fromMaybe Map.empty
                               . (fmap . fmap) go
  where
    go :: Normalizer.TypeProps -> FieldProps
    go = classifyTypeProps classifyUnionExpr
                           classifyCustomObject
                           classifyCustomString
                           classifyCustomUnion
                           symbolTable
    
    classifyUnionExpr :: UnionProps -> FieldProps
    classifyUnionExpr = CustomField . UnionTypeProps
    
    classifyCustomObject :: ObjectType
                         -> Map PropertyName FieldProps
                         -> Maybe Discriminator
                         -> FieldProps
    classifyCustomObject (ObjectRef typeName) props Nothing | Map.null props =
        RegularField typeName
    classifyCustomObject objectType props discr =
        CustomField $ ObjectTypeProps
                    $ ObjectProps
                    { parentObjectType = objectType
                    , properties = props
                    , objectDiscriminator = discr
                    }
    
    classifyCustomString :: StringType -> Maybe Regexp -> FieldProps
    classifyCustomString (StringRef typeName) Nothing =
        RegularField typeName
    classifyCustomString stringType pattern =
        CustomField $ StringTypeProps
                    $ StringProps
                    { parentStringType = stringType
                    , stringPattern = pattern
                    }
    
    classifyCustomUnion :: UnionType -> FieldProps
    classifyCustomUnion (UnionRef typeName) =
        RegularField typeName


classifyUnionBranch :: SymbolTable -> Normalizer.TypeExpr -> UnionBranch
classifyUnionBranch symbolTable = go
  where
    go :: Normalizer.TypeExpr -> UnionBranch
    go Normalizer.Object = ObjectBranch Object
    go Normalizer.String = StringBranch String
    go (Normalizer.Ref x) = case symbolTable ! x of
        ObjectTypeProps _ -> ObjectBranch (ObjectRef x)
        StringTypeProps _ -> StringBranch (StringRef x)
        UnionTypeProps _ -> error $ printf "unsupported: nested union type %s" (show x)
    go (Normalizer.Union xs) = error $ printf "unsupported: nested union type %s" (show xs)

classifyTypeExpr :: SymbolTable -> Normalizer.TypeExpr -> Either UnionProps Type
classifyTypeExpr symbolTable = go
  where
    go :: Normalizer.TypeExpr -> Either UnionProps Type
    go Normalizer.Object = Right $ ObjectType Object
    go Normalizer.String = Right $ StringType String
    go (Normalizer.Ref typeName) = case symbolTable ! typeName of
        ObjectTypeProps _ -> Right $ ObjectType $ ObjectRef typeName
        StringTypeProps _ -> Right $ StringType $ StringRef typeName
        UnionTypeProps _ -> Right $ UnionType $ UnionRef typeName
    go (Normalizer.Union branches) = Left
                                   $ UnionProps
                                   $ map (classifyUnionBranch symbolTable)
                                   $ branches


classifyTypeProps :: (UnionProps -> a)
                  -> (ObjectType -> Map PropertyName FieldProps
                                 -> Maybe Discriminator
                                 -> a)
                  -> (StringType -> Maybe Regexp
                                 -> a)
                  -> (UnionType -> a)
                  -> SymbolTable -> Normalizer.TypeProps -> a
classifyTypeProps classifyUnionExpr
                  classifyCustomObject
                  classifyCustomString
                  classifyCustomUnion
                  symbolTable
                  typeProps = case (parentType, typeProps) of
    (Right (ObjectType objectType),
     Normalizer.TypeProps _ props discr Nothing) ->
      classifyCustomObject objectType
                           (classifyProperties symbolTable props)
                           discr
    (Right (ObjectType _),
     Normalizer.TypeProps _ _ _ (Just pattern)) ->
      error $ printf "illegal: pattern %s in object type"
                     (show pattern)
    (Right (StringType stringType),
     Normalizer.TypeProps _ Nothing Nothing pattern) ->
      classifyCustomString stringType
                           pattern
    (Right (StringType _),
     Normalizer.TypeProps _ (Just props) _ _) ->
      error $ printf "illegal: properties %s in string type"
                     (show props)
    (Right (StringType _),
     Normalizer.TypeProps _ _ (Just discr) _) ->
      error $ printf "illegal: discriminator %s in string type"
                     (show discr)
    (_,
     Normalizer.TypeProps _ (Just props) _ _) ->
      error $ printf "illegal: properties %s in union type"
                     (show props)
    (_,
     Normalizer.TypeProps _ _ (Just discr) _) ->
      error $ printf "illegal: discr %s in union type"
                     (show discr)
    (_,
     Normalizer.TypeProps _ _ _ (Just pattern)) ->
      error $ printf "illegal: pattern %s in union type"
                     (show pattern)
    (Right (UnionType unionType),
     Normalizer.TypeProps _ Nothing Nothing Nothing) ->
      classifyCustomUnion unionType
    (Left unionProps,
     Normalizer.TypeProps _ Nothing Nothing Nothing) ->
      classifyUnionExpr unionProps
  where
    parentTypeExpr :: Normalizer.TypeExpr
    parentTypeExpr = Normalizer.parentType typeProps
    
    parentType :: Either UnionProps Type
    parentType = classifyTypeExpr symbolTable parentTypeExpr

classifyTopLevelTypeProps :: SymbolTable -> Normalizer.TypeProps -> TypeProps
classifyTopLevelTypeProps = classifyTypeProps classifyUnionExpr
                                              classifyCustomObject
                                              classifyCustomString
                                              classifyCustomUnion
  where
    classifyUnionExpr :: UnionProps -> TypeProps
    classifyUnionExpr = UnionTypeProps
    
    classifyCustomObject :: ObjectType
                         -> Map PropertyName FieldProps
                         -> Maybe Discriminator
                         -> TypeProps
    classifyCustomObject objectType props discr =
        ObjectTypeProps $ ObjectProps
                        { parentObjectType = objectType
                        , properties = props
                        , objectDiscriminator = discr
                        }
    
    classifyCustomString :: StringType -> Maybe Regexp -> TypeProps
    classifyCustomString stringType pattern =
        StringTypeProps $ StringProps
                        { parentStringType = stringType
                        , stringPattern = pattern
                        }
    
    classifyCustomUnion :: UnionType -> TypeProps
    classifyCustomUnion unionType =
        error $ printf "unsupported: inheriting from union type %s"
                       (show unionType)


-- |
-- >>> import Raml.Parser
-- >>> import Raml.Normalizer
-- >>> r <- classify <$> normalize <$> parse <$> readYaml "tests/sample.in"
-- >>> printAsYaml r
-- types:
--   BooleanType:
--     type: Alternative
--     properties: {}
--   DateType:
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
--       dataType: DataType
--   NumberType:
--     type: Alternative
--     properties: {}
--   StringType:
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
        newSymbolTable = fmap (classifyTopLevelTypeProps newSymbolTable)
                              oldSymbolTable
