module Raml.Generator where

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
  } deriving (Show, Eq)


data Field = Field
  { fieldName :: ValName
  , fieldType :: TypeName
  } deriving (Show, Eq)

data CaseClass = CaseClass
  { caseClassName :: TypeName
  , parameters :: [Field]
  , requirements :: [Expr]
  } deriving (Show, Eq)


data Val = Val
  { valName :: ValName
  , valValue :: Expr
  } deriving (Show, Eq)

data CompanionObject = CompanionObject
  { staticVals :: [Val]
  } deriving (Show, Eq)

data GeneratedCode
  = GeneratedTrait Trait
  | GeneratedCaseObject CaseObject
  | GeneratedCaseClass CaseClass
  | GeneratedCompanionObject
  deriving (Show, Eq)

newtype GeneratedTree = GeneratedTree
  { unGeneratedTree :: [[GeneratedCode]]
  } deriving (Show, Eq)


generate :: AnalyzedTree -> GeneratedTree
generate _ = undefined
