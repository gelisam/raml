{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Language.Scala.Generator where

import Prelude hiding (null)

import           Data.AList (AList)
import qualified Data.AList as AList
import           Data.Yaml.Ordered (ToJSON(..))

import Data.Empty
import Data.IndentedCode
import Data.Yaml.Ordered.MyExtra
import Language.Scala.AnnotatedTree
import Language.Scala.CodeOverlay
import Language.Scala.Name


type OverlaidTree = AnnotatedTree CodeOverlay CodeOverlay () CodeLayout


data Trait = Trait
  { traitName :: TypeName
  , traitCode :: CodeLayout
  } deriving (Show, Eq)

data CaseObject = CaseObject
  { caseObjectName :: TypeName
  , caseObjectParent :: Maybe TypeName
  , caseObjectCode :: CodeLayout
  } deriving (Show, Eq)


data Field = Field
  { fieldName :: ValName
  , fieldType :: TypeName
  } deriving (Show, Eq)

data CaseClass = CaseClass
  { caseClassName :: TypeName
  , caseClassParent :: Maybe TypeName
  , caseClassFields :: [Field]
  , caseClassCode :: CodeLayout
  } deriving (Show, Eq)


data CompanionObject = CompanionObject
  { companionName :: TypeName
  , companionCode :: CodeLayout
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
  toJSON (Trait {..}) =
    object [ "trait" .=! object [ "name" .=! traitName
                                , "code" .=? traitCode
                                ]
           ]

instance ToJSON CaseObject where
  toJSON (CaseObject {..}) =
    object [ "case_object" .=! object [ "name"   .=! caseObjectName
                                      , "parent" .=? caseObjectParent
                                      , "code"   .=? caseObjectCode
                                      ]
           ]

instance ToJSON Field where
  toJSON (Field {..}) =
    object [ "field" .=! object [ "name" .=! fieldName
                                , "type" .=! fieldType
                                ]
           ]

instance ToJSON CaseClass where
  toJSON (CaseClass {..}) =
    object [ "case_class" .=! object [ "name"   .=! caseClassName
                                     , "parent" .=? caseClassParent
                                     , "fields" .=? caseClassFields
                                     , "code"   .=? caseClassCode
                                     ]
           ]

instance ToJSON CompanionObject where
  toJSON (CompanionObject {..}) =
    object [ "companion_object" .=! object [ "name" .=! companionName
                                           , "code" .=? companionCode
                                           ]
           ]

instance ToJSON GeneratedCode where
  toJSON (GeneratedTrait x) = toJSON x
  toJSON (GeneratedCaseObject x) = toJSON x
  toJSON (GeneratedCaseClass x) = toJSON x
  toJSON (GeneratedCompanionObject x) = toJSON x

instance Empty GeneratedCode where
  null (GeneratedCompanionObject (CompanionObject _ codeLayout)) = null codeLayout
  null _ = False

instance ToJSON GeneratedTree where
  toJSON (GeneratedTree x) = toJSON x


generateField :: ValName
              -> (TypeName, ())
              -> Field
generateField fieldName (fieldType, ()) = Field {..}

generateCaseClass :: Maybe TypeName
                  -> TypeName
                  -> CodeLayout
                  -> AList ValName (TypeName, ())
                  -> [GeneratedCode]
generateCaseClass parentName typeName codeLayout fields =
    [ GeneratedCaseClass
    $ CaseClass
    { caseClassName   = typeName
    , caseClassParent = parentName
    , caseClassFields = map (uncurry generateField) (AList.toList fields)
    , caseClassCode   = codeLayout
    }
    ]


generateBranch :: TypeName
               -> TypeName
               -> (BranchProps (), CodeLayout)
               -> [GeneratedCode]
generateBranch parentName branchName (BranchProps fields, codeLayout) =
    generateCaseClass (Just parentName) branchName codeLayout fields

generateSum :: TypeName
            -> CodeLayout
            -> SumProps () CodeLayout
            -> [GeneratedCode]
generateSum typeName codeLayout (SumProps branches) =
    [ GeneratedTrait
    $ Trait typeName codeLayout
    ] ++
    foldMap (uncurry (generateBranch typeName)) (AList.toList branches)

generateProduct :: TypeName
                -> CodeLayout
                -> ProductProps ()
                -> [GeneratedCode]
generateProduct typeName codeLayout (ProductProps branches) =
    generateCaseClass Nothing typeName codeLayout branches


generateTopLevel :: TypeName
                 -> TopLevelType CodeOverlay CodeOverlay () CodeLayout
                 -> [[GeneratedCode]]
generateTopLevel typeName (TopLevelProduct (productProps, CodeOverlay {..})) =
    [ generateProduct typeName overlaidMethods productProps
    , [GeneratedCompanionObject $ CompanionObject typeName overlaidHelpers]
    ]
generateTopLevel typeName (TopLevelSum (sumProps, CodeOverlay {..})) =
    [ generateSum typeName overlaidMethods sumProps
    , [GeneratedCompanionObject $ CompanionObject typeName overlaidHelpers]
    ]


-- |
-- >>> import Raml
-- >>> import Language.Scala.Annotator
-- >>> import Language.Scala.Converter
-- >>> r <- generate <$> annotate mempty mempty <$> convert <$> readRaml "tests/sample.in"
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
--         fields:
--         - field:
--             name: dateFormat
--             type: String
--     - case_class:
--         name: BooleanType
--         parent: DataType
--   - - companion_object:
--         name: DataType
-- - - - case_class:
--         name: Field
--         fields:
--         - field:
--             name: name
--             type: String
--         - field:
--             name: dataType
--             type: DataType
--   - - companion_object:
--         name: Field
generate :: OverlaidTree -> GeneratedTree
generate = GeneratedTree
         . map (uncurry generateTopLevel)
         . AList.toList
         . unAnnotatedTree
