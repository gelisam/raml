{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Language.Scala.Annotator where

import qualified Data.Foldable as Foldable

import Data.AList
import Language.Scala.AnnotatedTree
import Language.Scala.Converter
import Language.Scala.Name


data ProductPrefix = ProductPrefix
  { productName :: TypeName
  } deriving (Show, Eq)

data SumPrefix = SumPrefix
  { sumName :: TypeName
  } deriving (Show, Eq)

data BranchPrefix = BranchPrefix
  { branchContainer :: SumPrefix
  , branchName :: TypeName
  } deriving (Show, Eq)


data FieldContainer
  = FieldContainingProduct ProductPrefix
  | FieldContainingBranch BranchPrefix
  deriving (Show, Eq)

data FieldPrefix = FieldPrefix
  { fieldContainer :: FieldContainer
  , fieldName :: ValName
  } deriving (Show, Eq)



data ProductPath = ProductPath
  { productPrefix :: ProductPrefix
  , productProps :: ProductProps FieldAnnotation
  } deriving (Show, Eq)

data SumPath = SumPath
  { sumPrefix :: SumPrefix
  , sumProps :: SumProps FieldAnnotation ()
  } deriving (Show, Eq)

data FieldPath = FieldPath
  { fieldPrefix :: FieldPrefix
  , fieldProps :: (TypeName, FieldAnnotation)
  } deriving (Show, Eq)

data BranchPath = BranchPath
  { branchPrefix :: BranchPrefix
  , branchProps :: BranchProps FieldAnnotation
  } deriving (Show, Eq)


data TopLevelPath
  = TopLevelProductPath ProductPath
  | TopLevelSumPath SumPath
  deriving (Show, Eq)


type Annotator = (->)
type ProductAnnotator  = Annotator ProductPath
type SumAnnotator      = Annotator SumPath
type FieldAnnotator    = Annotator FieldPath
type BranchAnnotator   = Annotator BranchPath
type TopLevelAnnotator = Annotator TopLevelPath


groupProductFields :: forall a
                    . FieldAnnotator a
                   -> ProductAnnotator [a]
groupProductFields f (ProductPath {..}) = Foldable.toList
                                        $ mapWithKey go
                                        $ productFields productProps
  where
    go :: ValName -> (TypeName, FieldAnnotation) -> a
    go fieldName fieldProps = f (FieldPath {..})
      where
        fieldContainer :: FieldContainer
        fieldContainer = FieldContainingProduct productPrefix
        
        fieldPrefix :: FieldPrefix
        fieldPrefix = FieldPrefix {..}

groupBranchFields :: forall a
                   . FieldAnnotator a
                  -> BranchAnnotator [a]
groupBranchFields f (BranchPath {..}) = Foldable.toList
                                      $ mapWithKey go
                                      $ branchFields branchProps
  where
    go :: ValName -> (TypeName, FieldAnnotation) -> a
    go fieldName fieldProps = f (FieldPath {..})
      where
        fieldContainer :: FieldContainer
        fieldContainer = FieldContainingBranch branchPrefix
        
        fieldPrefix :: FieldPrefix
        fieldPrefix = FieldPrefix {..}

groupBranches :: forall a
               . BranchAnnotator a
              -> SumAnnotator [a]
groupBranches f (SumPath {..}) = Foldable.toList
                               $ mapWithKey go
                               $ branches sumProps
  where
    go :: TypeName -> (BranchProps FieldAnnotation, ()) -> a
    go branchName (branchProps, ()) = f (BranchPath {..})
      where
        branchContainer :: SumPrefix
        branchContainer = sumPrefix
        
        branchPrefix :: BranchPrefix
        branchPrefix = BranchPrefix {..}

groupTopLevels :: forall a
                . ProductAnnotator a
               -> SumAnnotator a
               -> TopLevelAnnotator a
groupTopLevels f g = go
  where
    go :: TopLevelPath -> a
    go (TopLevelProductPath productPath) = f productPath
    go (TopLevelSumPath     sumPath    ) = g sumPath

groupFields :: forall a
             . FieldAnnotator a
            -> TopLevelAnnotator [a]
groupFields f = go
  where
    go :: TopLevelPath -> [a]
    go (TopLevelProductPath productPath) = groupProductFields f productPath
    go (TopLevelSumPath sumPath) = concat $ groupBranches (groupBranchFields f) sumPath


annotate :: forall a
          . TopLevelAnnotator a
         -> ConvertedTree
         -> AnnotatedTree a a () ()
annotate f (AnnotatedTree xs) = AnnotatedTree (mapWithKey go xs)
  where
    go :: TypeName
       -> TopLevelType () () FieldAnnotation ()
       -> TopLevelType a a () ()
    go productName (TopLevelProduct productProps) = TopLevelProduct (annotateProduct (ProductPrefix {..}) productProps)
    go sumName     (TopLevelSum     sumProps    ) = TopLevelSum     (annotateSum     (SumPrefix     {..}) sumProps    )
    
    annotateProduct :: ProductPrefix
                    -> (ProductProps FieldAnnotation, ())
                    -> (ProductProps (), a)
    annotateProduct productPrefix (productProps, ()) = (productProps', f topLevelPath)
      where
        productProps' :: ProductProps ()
        productProps' = mapProductProps (const ()) productProps
        
        topLevelPath :: TopLevelPath
        topLevelPath = TopLevelProductPath (ProductPath {..})
    
    annotateSum :: SumPrefix
                -> (SumProps FieldAnnotation (), ())
                -> (SumProps () (), a)
    annotateSum sumPrefix (sumProps, ()) = (sumProps', f topLevelPath)
      where
        sumProps' :: SumProps () ()
        sumProps' = mapSumProps (const ()) (const ()) sumProps
        
        topLevelPath :: TopLevelPath
        topLevelPath = TopLevelSumPath (SumPath {..})
