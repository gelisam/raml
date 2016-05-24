{-# LANGUAGE OverloadedStrings #-}
module Raml.Analyzer where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Yaml.Ordered (ToJSON(..))
import qualified Data.Yaml.Ordered as Yaml
import Text.Printf

import Data.Yaml.Ordered.MyExtra
import Raml.Common
import           Raml.Classifier (ClassifiedTree(..))
import qualified Raml.Classifier as Classifier
import           Raml.UsageCounter


-- The goal of the analysis phase is to determine the purpose of each type.
-- The possible purposes are:
-- 
-- - RegularField, a named type.
-- - StringField, an anonymous String.
--   One day we'd like to support named String types for shared validation,
--   and also fields of types other than String, but for now only string
--   fields are allowed to be customized.
-- 
-- - Branch, an Object which appears in exactly one Union.
-- - Dummy, an Object with no properties.
--   This makes it easier to define sum types in RAML, by giving branches a
--   dummy parent which defines a discriminator instead of duplicating the
--   discriminator definition in each branch.
-- 
-- - NamedSum, a Union of Branches.
-- - AnonymousSum, any other Union (not yet supported).
--   The difference is that since Branches aren't used anywhere else, it
--   makes sense to define an algebraic datatype in which the Branch names
--   are the constructor names and the Branch properties are the arguments
--   to those constructors. When one of the alternatives is used in many
--   places, we instead interpret the Union as the (possibly-nested) Either
--   of multiple alternatives.
-- 
-- - NamedProduct, any other Object.
--   An AnonymousProduct would be a tuple, but there's no way to describe
--   one in RAML.


data BuiltinType
  = String
  deriving (Show, Eq)

data StringFieldProps = StringFieldProps
  { stringPattern :: Regexp
  } deriving (Show, Eq)

data Field
  = RegularField TypeName
  | BuiltinField BuiltinType
  | CustomStringField StringFieldProps
  deriving (Show, Eq)


data BranchProps = BranchProps
  { branchFields :: Map PropertyName Field
  } deriving (Show, Eq)

data DummyProps = DummyProps
  deriving (Show, Eq)

data NamedSumProps = NamedSumProps
  { sumBranches :: Map BranchName BranchProps
  } deriving (Show, Eq)

data NamedProductProps = NamedProductProps
  { productFields :: Map PropertyName Field
  } deriving (Show, Eq)

data TypeProps
  = NamedSumTypeProps NamedSumProps
  | NamedProductTypeProps NamedProductProps
  deriving (Show, Eq)


data Purpose
  = Dummy
  | Branch BranchName BranchProps
  | TopLevelType TypeProps
type SymbolTable = Map TypeName Purpose

data AnalyzedTree = AnalyzedTree
  { unAnalyzedTree :: Map TypeName TypeProps
  } deriving (Show, Eq)


instance ToJSON StringFieldProps where
  toJSON (StringFieldProps pattern) = object [ "type" .=! YamlString "string"
                                             , "pattern" .=! pattern
                                             ]

instance ToJSON Field where
  toJSON (RegularField x) = YamlString x
  toJSON (BuiltinField String) = YamlString "string"
  toJSON (CustomStringField x) = toJSON x

instance ToJSON BranchProps where
  toJSON (BranchProps props) = object [ "properties" .=? props
                                      ]

instance ToJSON DummyProps where
  toJSON DummyProps = Yaml.Null

instance ToJSON NamedSumProps where
  toJSON (NamedSumProps branches) = toJSON branches

instance ToJSON NamedProductProps where
  toJSON (NamedProductProps fields) = object [ "properties" .=? fields
                                             ]

instance ToJSON TypeProps where
  toJSON (NamedSumTypeProps x) = toJSON x
  toJSON (NamedProductTypeProps x) = toJSON x

instance ToJSON AnalyzedTree where
  toJSON (AnalyzedTree types) = object [ "types" .=! types
                                       ]


analyzeObjectField :: Classifier.ObjectProps -> Field
analyzeObjectField objectProps =
    error $ printf "unsupported: custom object field %s"
                   (show objectProps)

analyzeStringField :: Classifier.StringProps -> Field
analyzeStringField (Classifier.StringProps Classifier.String Nothing) =
    BuiltinField String
analyzeStringField (Classifier.StringProps Classifier.String (Just pattern)) =
    CustomStringField $ StringFieldProps pattern
analyzeStringField (Classifier.StringProps (Classifier.StringRef typeName) _) =
    error $ printf "unsupported: custom string field with custom parent %s"
                   (show typeName)

analyzeUnionField :: Classifier.UnionProps -> Field
analyzeUnionField unionProps =
    error $ printf "unsupported: union field %s" (show unionProps)

analyzeField :: Classifier.FieldProps -> Field
analyzeField (Classifier.RegularField typeName) = RegularField typeName
analyzeField (Classifier.CustomField (Classifier.ObjectTypeProps objectProps)) =
    analyzeObjectField objectProps
analyzeField (Classifier.CustomField (Classifier.StringTypeProps stringProps)) =
    analyzeStringField stringProps
analyzeField (Classifier.CustomField (Classifier.UnionTypeProps unionProps)) =
    analyzeUnionField unionProps

analyzeNamedProduct :: Map PropertyName Classifier.FieldProps -> TypeProps
analyzeNamedProduct = NamedProductTypeProps
                    . NamedProductProps
                    . fmap analyzeField


analyzeObjectBranch :: SymbolTable
                    -> Classifier.ObjectType
                    -> Maybe (BranchName, BranchProps)
analyzeObjectBranch symbolTable = go
  where
    go :: Classifier.ObjectType -> Maybe (BranchName, BranchProps)
    go Classifier.Object = Nothing
    go (Classifier.ObjectRef typeName) =
        case symbolTable Map.! typeName of
          Branch branchName branchProps -> Just (branchName, branchProps)
          _ -> Nothing

analyzeStringBranch :: Classifier.StringType -> Maybe (BranchName, BranchProps)
analyzeStringBranch _ = Nothing

-- Named Branch (Just) or Anonymous Alternative (Nothing)
analyzeBranch :: SymbolTable
              -> Classifier.UnionBranch
              -> Maybe (BranchName, BranchProps)
analyzeBranch symbolTable = go
  where
    go :: Classifier.UnionBranch -> Maybe (BranchName, BranchProps)
    go (Classifier.ObjectBranch x) = analyzeObjectBranch symbolTable x
    go (Classifier.StringBranch x) = analyzeStringBranch x


analyzeObjectProps :: UnionUsage
                   -> SymbolTable
                   -> TypeName
                   -> Classifier.ObjectProps
                   -> Purpose
analyzeObjectProps unionUsage symbolTable typeName
                   (Classifier.ObjectProps parent props discr) =
    case (unionUsage ! typeName, Map.size props, parent, discr) of
      (1,_,_,_) ->
        Branch typeName $ BranchProps
                        $ fmap analyzeField props
      (_,0,_,_) ->
        Dummy
      (_,_,Classifier.Object,Nothing) ->
        TopLevelType $ analyzeNamedProduct props
      (_,_,Classifier.ObjectRef parentName,Nothing) ->
        case symbolTable Map.! parentName of
          Dummy ->  -- Dummy parent, ignore
            TopLevelType $ analyzeNamedProduct props
          _ ->
            error $ printf "unsupported: named product with parent %s"
                           (show parentName)
      (_,_,_,Just d) ->
          error $ printf "unsupported: named product with discriminator %s"
                         (show d)

analyzeStringProps :: Classifier.StringProps
                   -> TypeProps
analyzeStringProps s = error
                     $ printf "unsupported: top-level String type %s" (show s)

analyzeUnionProps :: SymbolTable
                  -> Classifier.UnionProps
                  -> TypeProps
analyzeUnionProps symbolTable (Classifier.UnionProps oldBranches) =
    case traverse (analyzeBranch symbolTable) oldBranches of
      Nothing -> 
        error $ printf "unsupported: anonymous sum %s"
                       (show oldBranches)
      Just newBranches ->
        NamedSumTypeProps $ NamedSumProps
                          $ Map.fromList
                          $ newBranches

analyzeTypeProps :: UnionUsage
                 -> SymbolTable
                 -> TypeName
                 -> Classifier.TypeProps
                 -> Purpose
analyzeTypeProps unionUsage symbolTable typeName = go
  where
    go :: Classifier.TypeProps -> Purpose
    go (Classifier.ObjectTypeProps objectProps) =
        analyzeObjectProps unionUsage symbolTable typeName objectProps
    go (Classifier.StringTypeProps stringProps) =
        TopLevelType $ analyzeStringProps stringProps
    go (Classifier.UnionTypeProps unionProps) =
        TopLevelType $ analyzeUnionProps symbolTable unionProps


-- |
-- >>> import Raml.Parser
-- >>> import Raml.Normalizer
-- >>> import Raml.Classifier
-- >>> r <- analyze <$> Classifier.classify <$> normalize <$> parse <$> readYaml "tests/sample.in"
-- >>> printAsYaml r
-- types:
--   Field:
--     properties:
--       name: string
--       dataType: DataType
--   DataType:
--     BooleanType: {}
--     DateType:
--       properties:
--         dateFormat:
--           pattern: ! '[YMD]+[-\.][YMD]+[-\.\/][YMD]+'
--           type: string
--     NumberType: {}
--     StringType: {}
analyze :: ClassifiedTree -> AnalyzedTree
analyze = AnalyzedTree . Map.mapMaybe topLevelOnly . go . unClassifiedTree
  where
    topLevelOnly :: Purpose -> Maybe TypeProps
    topLevelOnly (TopLevelType typeProps) = Just typeProps
    topLevelOnly _ = Nothing
    
    go :: Classifier.SymbolTable -> SymbolTable
    go oldSymbolTable = newSymbolTable
      where
        unionUsage :: UnionUsage
        unionUsage = countUnionUsage oldSymbolTable
        
        newSymbolTable :: SymbolTable
        newSymbolTable = Map.mapWithKey f oldSymbolTable
        
        f :: TypeName
          -> Classifier.TypeProps
          -> Purpose
        f = analyzeTypeProps unionUsage newSymbolTable
