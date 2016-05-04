module Raml.Analyzer where

import           Data.Map (Map)

import           Raml.Common
import           Raml.Classifier (ClassifiedTree(..))


-- The goal of the analysis phase is to determine the purpose of each type.
-- The possible purposes are:
-- 
-- - StringField, an anonymous String.
--   One day we'd like to support named String types for shared validation,
--   and also fields of types other than String, but for now only string
--   fields are allowed.
-- 
-- - Branch, an Object which appears in exactly one Union.
-- - Dummy, an Object with no properties.
--   This makes it easier to define sum types in RAML, by giving branches a
--   dummy parent which defines a discriminator instead of duplicating the
--   discriminator definition in each branch.
-- 
-- - NamedSum, a Union of Branches.
-- - AnonymousSum, any other Union.
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


data StringFieldProps = StringFieldProps
  { stringPattern :: Maybe Regexp
  } deriving (Show, Eq)

data Field
  = RegularField TypeName
  | StringField StringFieldProps
  deriving (Show, Eq)


data BranchProps = BranchProps
  { branchFields :: Map PropertyName Field
  } deriving (Show, Eq)

data DummyProps = DummyProps
  deriving (Show, Eq)

data NamedSumProps = NamedSumProps
  { branches :: Map BranchName BranchProps
  , discriminator :: String
  } deriving (Show, Eq)

data NamedProductProps = NamedProductProps
  { fields :: [(PropertyName, Field)]
  } deriving (Show, Eq)

data TypeProps
  = NamedSumTypeProps NamedSumProps
  | NamedProductTypeProps NamedProductProps
  deriving (Show, Eq)


type SymbolTable = Map TypeName TypeProps

data AnalyzedTree = AnalyzedTree
  { unAnalyzedTree :: SymbolTable
  } deriving (Show, Eq)


analyze :: ClassifiedTree -> AnalyzedTree
analyze _ = undefined
