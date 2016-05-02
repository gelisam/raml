module Raml.Classifier where

import           Data.Map (Map)

import Raml.Common
import           Raml.Normalizer (NormalizedTree(..))


data ObjectType
  = Object
  | ObjectRef TypeName
  deriving (Show, Eq)

data StringType
  = String
  | StringRef TypeName
  deriving (Show, Eq)

type UnionType = [Either ObjectType StringType]

data Type
  = ObjectType ObjectType
  | StringType StringType
  | UnionType UnionType


data ObjectProps = ObjectProps
  { parentObjectType :: ObjectType
  , properties :: Map PropertyName TypeProps
  , objectDiscriminator :: Maybe PropertyName
  } deriving (Show, Eq)

data StringProps = StringProps
  { parentStringType :: StringType
  , stringPattern :: Maybe Regexp
  } deriving (Show, Eq)

type UnionProps = UnionType

data TypeProps
  = ObjectTypeProps ObjectProps
  | StringTypeProps StringProps
  | UnionTypeProps UnionProps
  deriving (Show, Eq)


newtype ClassifiedTree = ClassifiedTree
  { unClassifiedTree :: Map TypeName TypeProps
  } deriving (Show, Eq)


classify :: NormalizedTree -> ClassifiedTree
classify _ = undefined
