{-# LANGUAGE OverloadedStrings #-}
module Language.Scala.Converter where

import Language.Scala.Name
import Language.Scala.ScalaTree
import           Raml (Raml)
import qualified Raml as Raml


type FieldAnnotation = Maybe Raml.StringFieldProps

type ConvertedTree = ScalaTree () () FieldAnnotation ()


convertFieldType :: Raml.Field
                 -> (TypeName, FieldAnnotation)
convertFieldType (Raml.RegularField typeName) = (typeName, Nothing)
convertFieldType (Raml.BuiltinField Raml.String) = ("String", Nothing)
convertFieldType (Raml.CustomStringField extra) = ("String", Just extra)

convertProduct :: Raml.NamedProductProps
               -> Unannotated (ProductProps FieldAnnotation)
convertProduct = unannotated
               . ProductProps
               . fmap convertFieldType
               . Raml.productFields

convertBranch :: Raml.BranchProps
              -> Unannotated (BranchProps FieldAnnotation)
convertBranch = unannotated
              . BranchProps
              . fmap convertFieldType
              . Raml.branchFields

convertSum :: Raml.NamedSumProps
           -> Unannotated (SumProps FieldAnnotation ())
convertSum = unannotated
           . SumProps
           . fmap convertBranch
           . Raml.sumBranches

convertTypeProps :: Raml.TypeProps
                 -> TopLevelType () () FieldAnnotation ()
convertTypeProps (Raml.NamedProductTypeProps namedProduct) =
    TopLevelProduct (convertProduct namedProduct)
convertTypeProps (Raml.NamedSumTypeProps namedSum) =
    TopLevelSum (convertSum namedSum)


-- |
-- >>> import Raml
-- >>> import Data.Yaml.Ordered.MyExtra
-- >>> r <- convert <$> readRaml "tests/sample.in"
-- >>> printAsYaml r
-- DataType:
-- - StringType:
--   - fields: {}
--   - []
--   NumberType:
--   - fields: {}
--   - []
--   DateType:
--   - fields:
--       dateFormat:
--       - String
--       - type: string
--         pattern: ! '[YMD]+[-\.][YMD]+[-\.\/][YMD]+'
--   - []
--   BooleanType:
--   - fields: {}
--   - []
-- - []
-- Field:
-- - fields:
--     name:
--     - String
--     - null
--     dataType:
--     - DataType
--     - null
-- - []
convert :: Raml -> ConvertedTree
convert = ScalaTree
        . fmap convertTypeProps
        . Raml.unAnalyzedTree
