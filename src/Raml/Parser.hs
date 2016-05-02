module Raml.Parser where

import Data.Map as Map
import Data.Yaml as Yaml


type TypeName = String
type PropertyName = String
type Regexp = String

data TypeExpr
  = Object
  | String
  | Ref TypeName
  | Union [TypeExpr]
  deriving (Show, Eq)

data SharedProps = SharedProps
  { properties :: Map PropertyName PropertyProps
  , discriminator :: Maybe PropertyName
  , stringPattern :: Maybe Regexp
  } deriving (Show, Eq)

data PropertyProps = PropertyProps
  { type_ :: TypeExpr
  , sharedPropertyProps :: SharedProps
  } deriving (Show, Eq)

data TypeProps = TypeProps
  { parentType :: TypeExpr
  , sharedTypeProps :: SharedProps
  } deriving (Show, Eq)

newtype ParseTree = ParseTree
  { unParseTree :: Map TypeName TypeProps
  } deriving (Show, Eq)


parse :: Yaml.Value -> ParseTree
parse _ = undefined
