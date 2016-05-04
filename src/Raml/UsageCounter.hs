module Raml.UsageCounter where

import Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map

import Raml.Common
import Raml.Classifier


newtype UnionUsage = UnionUsage
  { getUnionUsage :: Map TypeName Int
  } deriving (Show, Eq)

instance Monoid UnionUsage where
  mempty = UnionUsage Map.empty
  UnionUsage xs `mappend` UnionUsage ys =
    UnionUsage (Map.unionWithKey (const (+)) xs ys)

countOnce :: TypeName -> UnionUsage
countOnce k = UnionUsage (Map.singleton k 1)

(!) :: UnionUsage -> TypeName -> Int
UnionUsage xs ! k = fromMaybe 0 (Map.lookup k xs)


countInObjectType :: ObjectType -> UnionUsage
countInObjectType Object = mempty
countInObjectType (ObjectRef k) = countOnce k

countInStringType :: StringType -> UnionUsage
countInStringType String = mempty
countInStringType (StringRef k) = countOnce k

countInUnionBranch :: UnionBranch -> UnionUsage
countInUnionBranch (ObjectBranch objectType) = countInObjectType objectType
countInUnionBranch (StringBranch stringType) = countInStringType stringType


countInObjectProps :: ObjectProps -> UnionUsage
countInObjectProps = foldMap countInTypeProps . properties

countInStringProps :: StringProps -> UnionUsage
countInStringProps _ = mempty

countInUnionProps :: UnionProps -> UnionUsage
countInUnionProps = foldMap countInUnionBranch


countInTypeProps :: TypeProps -> UnionUsage
countInTypeProps (ObjectTypeProps objectProps) = countInObjectProps objectProps
countInTypeProps (StringTypeProps stringProps) = countInStringProps stringProps
countInTypeProps (UnionTypeProps unionProps) = countInUnionProps unionProps


-- find in how many unions each type appears
countUnionUsage :: SymbolTable -> UnionUsage
countUnionUsage = foldMap countInTypeProps
