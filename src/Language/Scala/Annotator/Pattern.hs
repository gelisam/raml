module Language.Scala.Annotator.Pattern where

import Control.Applicative.Infix
import Control.Arrow ((***))
import Data.Functor.Compose
import Data.Maybe
import Text.Printf

import Data.IndentedCode
import qualified Raml as Raml
import Language.Scala.Annotator.DSL
import Language.Scala.CodeOverlay
import Language.Scala.Name


require :: CodeChunk -> CodeBlock
require (Line s) = CodeBlock
    [ Line $ printf "require(%s)" s
    ]
require (Indented ss) = CodeBlock
    [ Line "require("
    , Indented ss
    , Line ")"
    ]


type MaybeFieldAnnotator = Compose FieldAnnotator Maybe

annotator :: FieldAnnotator a -> MaybeFieldAnnotator a
annotator = Compose . fmap Just


pattern :: MaybeFieldAnnotator Raml.Regexp
pattern = Raml.stringPattern
      <$> Compose fieldAnnotation

patternVar :: FieldReader f => f Name
patternVar = capitalize <$> (fieldName <^(+++)^> pure (Name "pattern"))

qualifiedPatternVar :: FieldReader f => f Name
qualifiedPatternVar = topLevelName <^(.++)^> patternVar


condition :: MaybeFieldAnnotator CodeBlock
condition = CodeBlock
        <$> sequenceA
          [ Line <$> printf "%s match {" <$> fieldName
          , Indented <$> CodeBlock <$> sequenceA
            [ Line <$> printf "case %s() => true"
                   <$> nameToString
                   <$> qualifiedPatternVar
            , Line <$> pure   "case _ => false"
            ]
          , Line <$> pure   "}"
          ]

requireBlock :: MaybeFieldAnnotator CodeBlock
requireBlock = require <$> Indented <$> condition

declarationBlock :: MaybeFieldAnnotator CodeBlock
declarationBlock = singleLineBlock
               <$> ( printf "val %s = %s.r"
                 <$> (nameToString <$> patternVar)
                 <*> (show <$> pattern)
                   )


fieldContribution :: FieldAnnotator (CodeBlock, CodeBlock)
fieldContribution = fmap (fromMaybe mempty)
                  $ getCompose
                  $ requireBlock <^(,)^> declarationBlock

topLevelContribution :: TopLevelAnnotator (CodeBlock, CodeBlock)
topLevelContribution = mconcat <$> groupFields fieldContribution

patternOverlay :: TopLevelAnnotator CodeOverlay
patternOverlay = uncurry CodeOverlay
             <$> (singleBlockLayout *** singleBlockLayout)
             <$> topLevelContribution
