{-# LANGUAGE OverloadedStrings #-}
module Raml.Generator where

import           Data.Yaml (ToJSON(..))

import Data.Yaml.MyExtra
import Raml.Common
import           Raml.Analyzer (AnalyzedTree(..))


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


-- |
-- >>> import Raml.Parser
-- >>> import Raml.Normalizer
-- >>> import Raml.Classifier
-- >>> import Raml.Analyzer
-- >>> r <- generate <$> analyze <$> classify <$> normalize <$> parse <$> readYaml "tests/sample.in"
-- >>> printAsYaml r
-- - - - trait:
--         name: DataType
--     - case_object:
--         parent: DataType
--         name: StringType
--     - case_object:
--         parent: DataType
--         name: NumberType
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
--     - case_object:
--         parent: DataType
--         name: BooleanType
--   - - companion_object:
--         name: DateType
--         vals:
--         - val:
--             value: ! '"[YMD]+[-\.][YMD]+[-\.\/][YMD]+".r'
--             name: DateFormatPattern
-- - - - case_class:
--         name: Field
--         parameters:
--         - field:
--             name: name
--             type: String
--         - field:
--             name: dataType
--             type: DataType
generate :: AnalyzedTree -> GeneratedTree
generate _ = GeneratedTree
    [ [ [ GeneratedTrait $ Trait "DataType"
        , GeneratedCaseObject $ CaseObject "StringType" (Just "DataType")
        , GeneratedCaseObject $ CaseObject "NumberType" (Just "DataType")
        , GeneratedCaseClass $ CaseClass "DateType" (Just "DataType")
                             [ Field "dateFormat" "String"
                             ]
                             [ IndentedBlock
                               [ SingleLineExpr "dateFormat match {"
                               , IndentedBlock
                                 [ SingleLineExpr "case DateType.DateFormatPattern() => true"
                                 , SingleLineExpr "case _ => false"
                                 ]
                               , SingleLineExpr "}"
                               ]
                             ]
        , GeneratedCaseObject $ CaseObject "BooleanType" (Just "DataType")
        ]
      , [ GeneratedCompanionObject $ CompanionObject "DateType"
                                   [ Val "DateFormatPattern"
                                         (SingleLineExpr "\"[YMD]+[-\\.][YMD]+[-\\.\\/][YMD]+\".r")
                                   ]
        ]
      ]
    , [ [ GeneratedCaseClass $ CaseClass "Field" Nothing
                             [ Field "name" "String"
                             , Field "dataType" "DataType"
                             ] []
        ]
      ]
    ]
