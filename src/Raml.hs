module Raml where

import           Data.Yaml (Value)

import qualified Raml.Semantics as Semantics
import qualified Raml.Syntax as Syntax
import           Raml.Syntax (PrimitiveType(..))


data TypeDefinition
  = SumType SumDesc
  | ConcreteType ConcreteDesc
  deriving (Show, Eq)

data SumDesc = SumDesc
  { traitName :: String
  , alternatives :: [ConcreteDesc]
  } deriving (Show, Eq)

data ConcreteDesc
  = CaseObject ObjectDesc
  | CaseClass ClassDesc
  deriving (Show, Eq)

data ObjectDesc = ObjectDesc
  { objectName :: String
  } deriving (Show, Eq)

data ClassDesc = ClassDesc
  { className :: String
  , params :: [ParamDesc]
  } deriving (Show, Eq)

data ParamDesc = ParamDesc
  { paramName :: String
  , paramType :: ParamType
  , default_ :: Maybe Value
  } deriving (Show, Eq)

data ParamType
  = PrimitiveType PrimitiveType
  | NamedType String
  | MaybeType ParamType
  deriving (Show, Eq)
