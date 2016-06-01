{-# LANGUAGE OverloadedStrings #-}
module Language.Scala.Converter where

import           Data.AList (AList)
import           Data.Yaml.Ordered (ToJSON(..))

import Data.Yaml.Ordered.MyExtra
import Language.Scala.Name
import           Raml (Raml)
import qualified Raml as Raml


data ProductProps = ProductProps
  { productFields :: AList ValName TypeName
  } deriving (Show, Eq)

data BranchProps = BranchProps
  { branchFields :: AList ValName TypeName
  } deriving (Show, Eq)

data SumProps = SumProps
  { branches :: AList TypeName BranchProps
  } deriving (Show, Eq)


data TopLevelType
  = TopLevelProduct ProductProps
  | TopLevelSum SumProps
  deriving (Show, Eq)

newtype ConvertedTree = ConvertedTree
  { unConvertedTree :: AList TypeName TopLevelType
  } deriving (Show, Eq)


instance ToJSON BranchProps where
  toJSON (BranchProps x) = object [ "fields" .=! toJSON x ]

instance ToJSON SumProps where
  toJSON (SumProps x) = toJSON x

instance ToJSON ProductProps where
  toJSON (ProductProps x) = object [ "fields" .=! toJSON x ]

instance ToJSON TopLevelType where
  toJSON (TopLevelSum x) = toJSON x
  toJSON (TopLevelProduct x) = toJSON x

instance ToJSON ConvertedTree where
  toJSON (ConvertedTree x) = toJSON x


convertFieldType :: Raml.Field -> TypeName
convertFieldType (Raml.RegularField typeName) = typeName
convertFieldType (Raml.BuiltinField Raml.String) = "String"
convertFieldType (Raml.CustomStringField _) = "String"

convertProduct :: Raml.NamedProductProps -> ProductProps
convertProduct = ProductProps
               . fmap convertFieldType
               . Raml.productFields

convertBranch :: Raml.BranchProps -> BranchProps
convertBranch = BranchProps
              . fmap convertFieldType
              . Raml.branchFields

convertSum :: Raml.NamedSumProps -> SumProps
convertSum = SumProps
           . fmap convertBranch
           . Raml.sumBranches

convertTypeProps :: Raml.TypeProps -> TopLevelType
convertTypeProps (Raml.NamedProductTypeProps namedProduct) =
    TopLevelProduct (convertProduct namedProduct)
convertTypeProps (Raml.NamedSumTypeProps namedSum) =
    TopLevelSum (convertSum namedSum)


-- |
-- >>> import Raml
-- >>> r <- convert <$> readRaml "tests/sample.in"
-- >>> printAsYaml r
-- DataType:
--   StringType:
--     fields: {}
--   NumberType:
--     fields: {}
--   DateType:
--     fields:
--       dateFormat: String
--   BooleanType:
--     fields: {}
-- Field:
--   fields:
--     name: String
--     dataType: DataType
convert :: Raml -> ConvertedTree
convert = ConvertedTree
        . fmap convertTypeProps
        . Raml.unAnalyzedTree
