{-# LANGUAGE FlexibleInstances #-}
module Language.Scala.Annotator.DSL
  ( ProductAnnotator
  , SumAnnotator
  , FieldAnnotator
  , BranchAnnotator
  , TopLevelAnnotator
  
  , groupProductFields
  , groupBranchFields
  , groupBranches
  , groupTopLevels
  , groupFields
  
  , ProductReader(..)
  , SumReader(..)
  , FieldReader(..)
  , BranchReader(..)
  , TopLevelReader(..)
  
  , productName
  , sumName
  , fieldName
  , branchName
  , topLevelName
  
  , isProduct
  , isSum
  
  , fieldType
  , fieldAnnotation
  ) where

import Data.Functor.Compose

import Language.Scala.Converter
import qualified Language.Scala.Annotator as Annotator
import           Language.Scala.Annotator
  ( ProductAnnotator
  , SumAnnotator
  , FieldAnnotator
  , BranchAnnotator
  , TopLevelAnnotator
  
  , groupProductFields
  , groupBranchFields
  , groupBranches
  , groupTopLevels
  , groupFields
  )
import Language.Scala.Name


class TopLevelReader f => ProductReader f where
    productPrefix :: f Annotator.ProductPrefix

class TopLevelReader f => SumReader f where
    sumPrefix :: f Annotator.SumPrefix

class TopLevelReader f => FieldReader f where
    fieldPrefix :: f Annotator.FieldPrefix

class SumReader f => BranchReader f where
    branchPrefix :: f Annotator.BranchPrefix

class Applicative f => TopLevelReader f where
    topLevelPrefix :: f (Either Annotator.ProductPrefix
                                Annotator.SumPrefix)


instance ProductReader ProductAnnotator where
    productPrefix = productPrefix

instance TopLevelReader ProductAnnotator where
    topLevelPrefix = Left <$> productPrefix


instance SumReader SumAnnotator where
    sumPrefix = Annotator.sumPrefix

instance TopLevelReader SumAnnotator where
    topLevelPrefix = Right <$> sumPrefix


instance FieldReader FieldAnnotator where
    fieldPrefix = Annotator.fieldPrefix

instance TopLevelReader FieldAnnotator where
    topLevelPrefix = go
                <$> Annotator.fieldContainer
                <$> fieldPrefix
      where
        go :: Annotator.FieldContainer
           -> Either Annotator.ProductPrefix
                     Annotator.SumPrefix
        go (Annotator.FieldContainingProduct productPrefix_) =
            Left productPrefix_
        go (Annotator.FieldContainingBranch branchPrefix_) =
            Right (Annotator.branchContainer branchPrefix_)


instance BranchReader BranchAnnotator where
    branchPrefix = Annotator.branchPrefix

instance SumReader BranchAnnotator where
    sumPrefix = Annotator.branchContainer <$> branchPrefix

instance TopLevelReader BranchAnnotator where
    topLevelPrefix = Right <$> sumPrefix


instance TopLevelReader TopLevelAnnotator where
    topLevelPrefix (Annotator.TopLevelProductPath productPath) =
        Left (productPrefix productPath)
    topLevelPrefix (Annotator.TopLevelSumPath sumPath) =
        Right (sumPrefix sumPath)


instance (ProductReader f, Applicative g) => ProductReader (Compose f g) where
    productPrefix = Compose $ fmap pure $ productPrefix

instance (SumReader f, Applicative g) => SumReader (Compose f g) where
    sumPrefix = Compose $ fmap pure $ sumPrefix

instance (FieldReader f, Applicative g) => FieldReader (Compose f g) where
    fieldPrefix = Compose $ fmap pure $ fieldPrefix

instance (BranchReader f, Applicative g) => BranchReader (Compose f g) where
    branchPrefix = Compose $ fmap pure $ branchPrefix

instance (TopLevelReader f, Applicative g) => TopLevelReader (Compose f g) where
    topLevelPrefix = Compose $ fmap pure $ topLevelPrefix


productName :: ProductReader f => f TypeName
productName = Annotator.productName <$> productPrefix

sumName :: SumReader f => f TypeName
sumName = Annotator.sumName <$> sumPrefix

fieldName :: FieldReader f => f ValName
fieldName = Annotator.fieldName <$> fieldPrefix

branchName :: BranchReader f => f TypeName
branchName = Annotator.branchName <$> branchPrefix

topLevelName :: TopLevelReader f => f TypeName
topLevelName = go <$> topLevelPrefix
  where
    go :: Either Annotator.ProductPrefix
                 Annotator.SumPrefix
       -> TypeName
    go (Left productPrefix_) = Annotator.productName productPrefix_
    go (Right sumPrefix_) = Annotator.sumName sumPrefix_


isProduct :: TopLevelReader f => f Bool
isProduct = go <$> topLevelPrefix
  where
    go :: Either Annotator.ProductPrefix
                 Annotator.SumPrefix
       -> Bool
    go (Left _) = True
    go (Right _) = False

isSum :: TopLevelReader f => f Bool
isSum = not <$> isProduct


fieldType :: FieldAnnotator TypeName
fieldType = fst . Annotator.fieldProps

fieldAnnotation :: FieldAnnotator FieldAnnotation
fieldAnnotation = snd . Annotator.fieldProps
