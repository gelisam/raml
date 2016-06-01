{-# LANGUAGE OverloadedStrings #-}
module Language.Scala.Converter where

import Language.Scala.Name
import Language.Scala.ScalaTree
import           Raml (Raml)
import qualified Raml as Raml


type ConvertedTree = ScalaTree () () () ()


convertFieldType :: Raml.Field -> Unannotated TypeName
convertFieldType (Raml.RegularField typeName) = unannotated typeName
convertFieldType (Raml.BuiltinField Raml.String) = unannotated "String"
convertFieldType (Raml.CustomStringField _) = unannotated "String"

convertProduct :: Raml.NamedProductProps -> Unannotated (ProductProps ())
convertProduct = unannotated
               . ProductProps
               . fmap convertFieldType
               . Raml.productFields

convertBranch :: Raml.BranchProps -> Unannotated (BranchProps ())
convertBranch = unannotated
              . BranchProps
              . fmap convertFieldType
              . Raml.branchFields

convertSum :: Raml.NamedSumProps -> Unannotated (SumProps () ())
convertSum = unannotated
           . SumProps
           . fmap convertBranch
           . Raml.sumBranches

convertTypeProps :: Raml.TypeProps -> TopLevelType () () () ()
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
--       - []
--   - []
--   BooleanType:
--   - fields: {}
--   - []
-- - []
-- Field:
-- - fields:
--     name:
--     - String
--     - []
--     dataType:
--     - DataType
--     - []
-- - []
convert :: Raml -> ConvertedTree
convert = ScalaTree
        . fmap convertTypeProps
        . Raml.unAnalyzedTree
