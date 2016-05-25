module Raml.UsageCounter where

import Data.Maybe
import           Data.AList (AList)
import qualified Data.AList as AList

import Raml.Common
import Raml.Classifier


newtype UnionUsage = UnionUsage
  { getUnionUsage :: AList TypeName Int
  } deriving (Show, Eq)

instance Monoid UnionUsage where
  mempty = UnionUsage AList.empty
  UnionUsage xs `mappend` UnionUsage ys =
    UnionUsage (AList.unionWithKey (const (+)) xs ys)

countOnce :: TypeName -> UnionUsage
countOnce k = UnionUsage (AList.singleton k 1)

(!) :: UnionUsage -> TypeName -> Int
UnionUsage xs ! k = fromMaybe 0 (AList.lookup k xs)


countInObjectType :: ObjectType -> UnionUsage
countInObjectType Object = mempty
countInObjectType (ObjectRef k) = countOnce k

countInStringType :: StringType -> UnionUsage
countInStringType String = mempty
countInStringType (StringRef k) = countOnce k

countInUnionBranch :: UnionBranch -> UnionUsage
countInUnionBranch (ObjectBranch objectType) = countInObjectType objectType
countInUnionBranch (StringBranch stringType) = countInStringType stringType


countInFieldProps :: FieldProps -> UnionUsage
countInFieldProps (RegularField _) = mempty
countInFieldProps (CustomField typeProps) = countInTypeProps typeProps


countInObjectProps :: ObjectProps -> UnionUsage
countInObjectProps = foldMap countInFieldProps . properties

countInStringProps :: StringProps -> UnionUsage
countInStringProps _ = mempty

countInUnionProps :: UnionProps -> UnionUsage
countInUnionProps (UnionProps branches) = foldMap countInUnionBranch branches


countInTypeProps :: TypeProps -> UnionUsage
countInTypeProps (ObjectTypeProps objectProps) = countInObjectProps objectProps
countInTypeProps (StringTypeProps stringProps) = countInStringProps stringProps
countInTypeProps (UnionTypeProps unionProps) = countInUnionProps unionProps


-- find in how many unions each type appears
countUnionUsage :: SymbolTable -> UnionUsage
countUnionUsage = foldMap countInTypeProps
