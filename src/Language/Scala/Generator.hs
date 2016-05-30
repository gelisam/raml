{-# LANGUAGE OverloadedStrings #-}
module Language.Scala.Generator where

import Data.Maybe
import           Data.AList (AList)
import qualified Data.AList as AList
import           Data.Yaml.Ordered (ToJSON(..))
import Text.Printf

import Data.Empty
import Data.IndentedCode
import Data.Yaml.Ordered.MyExtra
import Language.Scala.Name
import Raml.Common
import           Raml.Analyzer (AnalyzedTree(..))
import qualified Raml.Analyzer as Analyzer


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
  , requirements :: [CodeChunk]
  } deriving (Show, Eq)


data Val = Val
  { valName :: ValName
  , valValue :: CodeChunk
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

instance Empty GeneratedCode where
  null (GeneratedCompanionObject (CompanionObject _ [])) = True
  null _ = False

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
                               -> CodeChunk
generateStringFieldRequirement companionNamer fieldName_ _ =
    Indented
    [ Line $ printf "%s match {" fieldName_
    , Indented
      [ Line $ printf "case %s() => true"
                                (nameToString patternVar)
      , Line "case _ => false"
      ]
    , Line "}"
    ]
  where
    patternVar :: Name
    patternVar = capitalize (companionNamer fieldName_ "pattern")

accompanyStringFieldRequirement :: CompanionNamer
                                -> PropertyName
                                -> Analyzer.StringFieldProps
                                -> [Val]
accompanyStringFieldRequirement companionNamer fieldName_
                                (Analyzer.StringFieldProps pattern) =
    [ Val
    { valName = nameToString patternVar
    , valValue = Line $ printf "%s.r" (show pattern)
    }
    ]
  where
    patternVar = capitalize (companionNamer fieldName_ "pattern")


generateRequirement :: CompanionNamer
                    -> PropertyName
                    -> Analyzer.Field
                    -> Maybe CodeChunk
generateRequirement companionNamer fieldName_ = go
  where
    go :: Analyzer.Field -> Maybe CodeChunk
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
    , parameters = map (uncurry generateField) (AList.toList fields)
    , requirements = mapMaybe (uncurry go) (AList.toList fields)
    }
    ]
  where
    go :: PropertyName -> Analyzer.Field -> Maybe CodeChunk
    go = generateRequirement companionNamer
    
    companionNamer :: CompanionNamer
    companionNamer = qualifiedCompanionNamer typeName

accompanyProductClass :: TypeName -> Analyzer.NamedProductProps -> [GeneratedCode]
accompanyProductClass typeName (Analyzer.NamedProductProps fields) =
    accompanyCaseClass unqualifiedCompanionNamer typeName fields


generateCaseClass :: CompanionNamer
                  -> TypeName
                  -> Maybe TypeName
                  -> AList PropertyName Analyzer.Field
                  -> [GeneratedCode]
generateCaseClass companionNamer typeName parentName fields =
    [ GeneratedCaseClass
    $ CaseClass
    { caseClassName = typeName
    , caseClassParent = parentName
    , parameters = map (uncurry generateField) (AList.toList fields)
    , requirements = mapMaybe (uncurry go) (AList.toList fields)
    }
    ]
  where
    go :: PropertyName -> Analyzer.Field -> Maybe CodeChunk
    go = generateRequirement companionNamer

accompanyCaseClass :: CompanionNamer
                   -> TypeName
                   -> AList PropertyName Analyzer.Field
                   -> [GeneratedCode]
accompanyCaseClass companionNamer typeName fields =
    [ GeneratedCompanionObject
    $ CompanionObject
    { companionName = typeName
    , staticVals = foldMap (uncurry go) (AList.toList fields)
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
    foldMap (uncurry (generateBranch typeName)) (AList.toList branches)

accompanyBranches :: TypeName -> Analyzer.NamedSumProps -> [GeneratedCode]
accompanyBranches typeName (Analyzer.NamedSumProps branches) =
    accompanyCaseClass unqualifiedCompanionNamer typeName AList.empty ++
    foldMap (uncurry accompanyBranch) (AList.toList branches)


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
--         name: StringType
--         parent: DataType
--     - case_class:
--         name: NumberType
--         parent: DataType
--     - case_class:
--         name: DateType
--         parent: DataType
--         parameters:
--         - field:
--             name: dateFormat
--             type: String
--         requirements:
--         - - dateFormat match {
--           - - case DateType.DateFormatPattern() => true
--             - case _ => false
--           - ! '}'
--     - case_class:
--         name: BooleanType
--         parent: DataType
--   - - companion_object:
--         name: DataType
--     - companion_object:
--         name: StringType
--     - companion_object:
--         name: NumberType
--     - companion_object:
--         name: DateType
--         vals:
--         - val:
--             name: DateFormatPattern
--             value: ! '"[YMD]+[-\\.][YMD]+[-\\.\\/][YMD]+".r'
--     - companion_object:
--         name: BooleanType
-- - - - case_class:
--         name: Field
--         parameters:
--         - field:
--             name: name
--             type: String
--         - field:
--             name: dataType
--             type: DataType
--   - - companion_object:
--         name: Field
generate :: AnalyzedTree -> GeneratedTree
generate = GeneratedTree
         . map (uncurry generateTypeProps)
         . AList.toList
         . unAnalyzedTree
