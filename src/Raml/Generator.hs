{-# LANGUAGE OverloadedStrings #-}
module Raml.Generator where

import Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Yaml (ToJSON(..))
import Text.Printf

import Data.Yaml.MyExtra
import Raml.Common
import Raml.ScalaName
import           Raml.Analyzer (AnalyzedTree(..))
import qualified Raml.Analyzer as Analyzer


data Expr
  = SingleLineExpr String
  | IndentedBlock [Expr]
  deriving (Show, Eq)


data Trait = Trait
  { traitName :: TypeName
  } deriving (Show, Eq)

data CaseObject = CaseObject
  { caseObjectName :: TypeName
  , caseObjectParent :: Maybe TypeName
  } deriving (Show, Eq)


data Field = Field
  { fieldName :: ValName
  , fieldType :: TypeName
  } deriving (Show, Eq)

data CaseClass = CaseClass
  { caseClassName :: TypeName
  , caseClassParent :: Maybe TypeName
  , parameters :: [Field]
  , requirements :: [Expr]
  } deriving (Show, Eq)


data Val = Val
  { valName :: ValName
  , valValue :: Expr
  } deriving (Show, Eq)

data CompanionObject = CompanionObject
  { companionName :: TypeName
  , staticVals :: [Val]
  } deriving (Show, Eq)

data GeneratedCode
  = GeneratedTrait Trait
  | GeneratedCaseObject CaseObject
  | GeneratedCaseClass CaseClass
  | GeneratedCompanionObject CompanionObject
  deriving (Show, Eq)

newtype GeneratedTree = GeneratedTree
  { unGeneratedTree :: [[[GeneratedCode]]]
  } deriving (Show, Eq)


instance ToJSON Expr where
  toJSON (SingleLineExpr x) = YamlString x
  toJSON (IndentedBlock xs) = toJSON xs

instance ToJSON Trait where
  toJSON (Trait name) =
    object [ "trait" .=! object [ "name" .=! name
                                ]
           ]

instance ToJSON CaseObject where
  toJSON (CaseObject name parent) =
    object [ "case_object" .=! object [ "name" .=! name
                                      , "parent" .=? parent
                                      ]
           ]

instance ToJSON Field where
  toJSON (Field name type_) =
    object [ "field" .=! object [ "name" .=! name
                                , "type" .=! type_
                                ]
           ]

instance ToJSON CaseClass where
  toJSON (CaseClass name parent params reqs) =
    object [ "case_class" .=! object [ "name" .=! name
                                     , "parent" .=? parent
                                     , "parameters" .=? params
                                     , "requirements" .=? reqs
                                     ]
           ]

instance ToJSON Val where
  toJSON (Val name value) =
    object [ "val" .=! object [ "name" .=! name
                              , "value" .=! value
                              ]
           ]

instance ToJSON CompanionObject where
  toJSON (CompanionObject name vals) =
    object [ "companion_object" .=! object [ "name" .=! name
                                           , "vals" .=? vals
                                           ]
           ]

instance ToJSON GeneratedCode where
  toJSON (GeneratedTrait x) = toJSON x
  toJSON (GeneratedCaseObject x) = toJSON x
  toJSON (GeneratedCaseClass x) = toJSON x
  toJSON (GeneratedCompanionObject x) = toJSON x

instance ToJSON GeneratedTree where
  toJSON (GeneratedTree x) = toJSON x


generateType :: Analyzer.Field -> TypeName
generateType (Analyzer.RegularField typeName) = typeName
generateType (Analyzer.BuiltinField Analyzer.String) = "String"
generateType (Analyzer.CustomStringField _) = "String"

generateField :: PropertyName -> Analyzer.Field -> Field
generateField fieldName_ type_ = Field fieldName_ (generateType type_)


generateStringFieldRequirement :: CompanionNamer
                               -> PropertyName
                               -> Analyzer.StringFieldProps
                               -> Expr
generateStringFieldRequirement companionNamer fieldName_ _ =
    IndentedBlock
    [ SingleLineExpr $ printf "%s match {" fieldName_
    , IndentedBlock
      [ SingleLineExpr $ printf "case %s() => true"
                                (nameToString patternVar)
      , SingleLineExpr "case _ => false"
      ]
    , SingleLineExpr "}"
    ]
  where
    patternVar :: ScalaName
    patternVar = capitalize (companionNamer fieldName_ "pattern")

accompanyStringFieldRequirement :: CompanionNamer
                                -> PropertyName
                                -> Analyzer.StringFieldProps
                                -> [Val]
accompanyStringFieldRequirement companionNamer fieldName_
                                (Analyzer.StringFieldProps pattern) =
    [ Val
    { valName = nameToString patternVar
    , valValue = SingleLineExpr $ printf "%s.r" (show pattern)
    }
    ]
  where
    patternVar = capitalize (companionNamer fieldName_ "pattern")


generateRequirement :: CompanionNamer
                    -> PropertyName
                    -> Analyzer.Field
                    -> Maybe Expr
generateRequirement companionNamer fieldName_ = go
  where
    go :: Analyzer.Field -> Maybe Expr
    go (Analyzer.RegularField _) = Nothing
    go (Analyzer.BuiltinField _) = Nothing
    go (Analyzer.CustomStringField stringFieldProps) =
        Just $ generateStringFieldRequirement companionNamer fieldName_ stringFieldProps

accompanyRequirement :: CompanionNamer
                     -> PropertyName
                     -> Analyzer.Field
                     -> [Val]
accompanyRequirement companionNamer fieldName_ = go
  where
    go :: Analyzer.Field -> [Val]
    go (Analyzer.RegularField _) = []
    go (Analyzer.BuiltinField _) = []
    go (Analyzer.CustomStringField customField) =
        accompanyStringFieldRequirement companionNamer fieldName_ customField


generateProductClass :: TypeName -> Analyzer.NamedProductProps -> [GeneratedCode]
generateProductClass typeName (Analyzer.NamedProductProps fields) =
    [ GeneratedCaseClass
    $ CaseClass
    { caseClassName = typeName
    , caseClassParent = Nothing
    , parameters = map (uncurry generateField) (Map.toList fields)
    , requirements = mapMaybe (uncurry go) (Map.toList fields)
    }
    ]
  where
    go :: PropertyName -> Analyzer.Field -> Maybe Expr
    go = generateRequirement companionNamer
    
    companionNamer :: CompanionNamer
    companionNamer = qualifiedCompanionNamer typeName

accompanyProductClass :: TypeName -> Analyzer.NamedProductProps -> [GeneratedCode]
accompanyProductClass typeName (Analyzer.NamedProductProps fields) =
    accompanyCaseClass unqualifiedCompanionNamer typeName fields


generateCaseClass :: CompanionNamer
                  -> TypeName
                  -> Maybe TypeName
                  -> Map PropertyName Analyzer.Field
                  -> [GeneratedCode]
generateCaseClass companionNamer typeName parentName fields =
    [ GeneratedCaseClass
    $ CaseClass
    { caseClassName = typeName
    , caseClassParent = parentName
    , parameters = map (uncurry generateField) (Map.toList fields)
    , requirements = mapMaybe (uncurry go) (Map.toList fields)
    }
    ]
  where
    go :: PropertyName -> Analyzer.Field -> Maybe Expr
    go = generateRequirement companionNamer

accompanyCaseClass :: CompanionNamer
                   -> TypeName
                   -> Map PropertyName Analyzer.Field
                   -> [GeneratedCode]
accompanyCaseClass companionNamer typeName fields =
    [ GeneratedCompanionObject
    $ CompanionObject
    { companionName = typeName
    , staticVals = foldMap (uncurry go) (Map.toList fields)
    }
    ]
  where
    go :: PropertyName -> Analyzer.Field -> [Val]
    go = accompanyRequirement companionNamer


generateBranch :: TypeName -> BranchName -> Analyzer.BranchProps -> [GeneratedCode]
generateBranch parentName branchName (Analyzer.BranchProps fields) =
    generateCaseClass companionNamer branchName (Just parentName) fields
  where
    companionNamer :: CompanionNamer
    companionNamer = qualifiedCompanionNamer branchName

accompanyBranch :: BranchName -> Analyzer.BranchProps -> [GeneratedCode]
accompanyBranch typeName (Analyzer.BranchProps fields) =
    accompanyCaseClass unqualifiedCompanionNamer typeName fields

generateBranches :: TypeName -> Analyzer.NamedSumProps -> [GeneratedCode]
generateBranches typeName (Analyzer.NamedSumProps branches) =
    [ GeneratedTrait
    $ Trait typeName
    ] ++
    foldMap (uncurry (generateBranch typeName)) (Map.toList branches)

accompanyBranches :: TypeName -> Analyzer.NamedSumProps -> [GeneratedCode]
accompanyBranches typeName (Analyzer.NamedSumProps branches) =
    accompanyCaseClass unqualifiedCompanionNamer typeName Map.empty ++
    foldMap (uncurry accompanyBranch) (Map.toList branches)


generateNamedSum :: TypeName -> Analyzer.NamedSumProps -> [[GeneratedCode]]
generateNamedSum typeName namedSum =
    [ generateBranches typeName namedSum
    , accompanyBranches typeName namedSum
    ]

generateNamedProduct :: TypeName -> Analyzer.NamedProductProps -> [[GeneratedCode]]
generateNamedProduct typeName namedProduct =
    [ generateProductClass typeName namedProduct
    , accompanyProductClass typeName namedProduct
    ]

generateTypeProps :: TypeName -> Analyzer.TypeProps -> [[GeneratedCode]]
generateTypeProps typeName (Analyzer.NamedSumTypeProps namedSum) =
    generateNamedSum typeName namedSum
generateTypeProps typeName (Analyzer.NamedProductTypeProps namedProduct) =
    generateNamedProduct typeName namedProduct


-- |
-- >>> import Raml.Parser
-- >>> import Raml.Normalizer
-- >>> import Raml.Classifier
-- >>> import Raml.Analyzer
-- >>> r <- generate <$> analyze <$> classify <$> normalize <$> parse <$> readYaml "tests/sample.in"
-- >>> printAsYaml r
-- - - - trait:
--         name: DataType
--     - case_class:
--         parent: DataType
--         name: BooleanType
--     - case_class:
--         parent: DataType
--         requirements:
--         - - dateFormat match {
--           - - case DateType.DateFormatPattern() => true
--             - case _ => false
--           - ! '}'
--         name: DateType
--         parameters:
--         - field:
--             name: dateFormat
--             type: String
--     - case_class:
--         parent: DataType
--         name: NumberType
--     - case_class:
--         parent: DataType
--         name: StringType
--   - - companion_object:
--         name: DataType
--     - companion_object:
--         name: BooleanType
--     - companion_object:
--         name: DateType
--         vals:
--         - val:
--             value: ! '"[YMD]+[-\\.][YMD]+[-\\.\\/][YMD]+".r'
--             name: DateFormatPattern
--     - companion_object:
--         name: NumberType
--     - companion_object:
--         name: StringType
-- - - - case_class:
--         name: Field
--         parameters:
--         - field:
--             name: dataType
--             type: DataType
--         - field:
--             name: name
--             type: String
--   - - companion_object:
--         name: Field
generate :: AnalyzedTree -> GeneratedTree
generate = GeneratedTree
         . map (uncurry generateTypeProps)
         . Map.toList
         . unAnalyzedTree
