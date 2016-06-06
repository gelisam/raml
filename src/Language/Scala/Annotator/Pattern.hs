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

-- $setup
-- >>> import qualified Language.Scala.Annotator as Annotator
-- >>> :{
-- let fieldPath = Annotator.FieldPath
--       (Annotator.FieldPrefix
--         (Annotator.FieldContainingProduct
--           (Annotator.ProductPrefix "VarNode"))
--         "identifier")
--       ( "String"
--       , Just (Raml.StringFieldProps "[a-zA-Z0-9_]*")
--       )
-- :}
-- 
-- >>> let testAnnotator f = f fieldPath
-- >>> let testMaybeAnnotator f = getCompose f fieldPath


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


-- |
-- >>> testMaybeAnnotator pattern
-- Just "[a-zA-Z0-9_]*"
pattern :: MaybeFieldAnnotator Raml.Regexp
pattern = Raml.stringPattern
      <$> Compose fieldAnnotation

-- |
-- >>> nameToString $ testAnnotator patternVar
-- "IdentifierPattern"
patternVar :: FieldReader f => f Name
patternVar = capitalize <$> (fieldName <^(+++)^> pure (Name "pattern"))

-- |
-- >>> nameToString $ testAnnotator qualifiedPatternVar
-- "VarNode.IdentifierPattern"
qualifiedPatternVar :: FieldReader f => f Name
qualifiedPatternVar = topLevelName <^(.++)^> patternVar


-- |
-- >>> testBlock $ fromJust $ testMaybeAnnotator condition
-- identifier match {
--   case VarNode.IdentifierPattern() => true
--   case _ => false
-- }
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

-- |
-- >>> testBlock $ fromJust $ testMaybeAnnotator requireBlock
-- require(
--   identifier match {
--     case VarNode.IdentifierPattern() => true
--     case _ => false
--   }
-- )
requireBlock :: MaybeFieldAnnotator CodeBlock
requireBlock = require <$> Indented <$> condition

-- |
-- >>> testBlock $ fromJust $ testMaybeAnnotator declarationBlock
-- val IdentifierPattern = "[a-zA-Z0-9_]*".r
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

branchAnnotator :: BranchAnnotator CodeLayout
branchAnnotator = multiBlockLayout
              <$> groupBranchFields (fst <$> fieldContribution)


topLevelContribution :: TopLevelAnnotator (CodeBlock, CodeBlock)
topLevelContribution = mconcat <$> groupFields fieldContribution

-- |
-- >>> import Data.Yaml.Ordered.MyExtra
-- >>> import Raml
-- >>> import Language.Scala.Converter
-- >>> r <- Annotator.annotate topLevelAnnotator branchAnnotator <$> convert <$> readRaml "tests/sample.in"
-- >>> printAsYaml r
-- DataType:
-- - StringType:
--   - fields: {}
--   - - []
--   NumberType:
--   - fields: {}
--   - - []
--   DateType:
--   - fields:
--       dateFormat:
--       - String
--       - []
--   - - - - require(
--         - - dateFormat match {
--           - - case DataType.DateFormatPattern() => true
--             - case _ => false
--           - ! '}'
--         - )
--   BooleanType:
--   - fields: {}
--   - - []
-- - methods:
--   - - - require(
--       - - dateFormat match {
--         - - case DataType.DateFormatPattern() => true
--           - case _ => false
--         - ! '}'
--       - )
--   helpers:
--   - - - val DateFormatPattern = "[YMD]+[-\\.][YMD]+[-\\.\\/][YMD]+".r
-- Field:
-- - fields:
--     name:
--     - String
--     - []
--     dataType:
--     - DataType
--     - []
-- - methods:
--   - - []
--   helpers:
--   - - []
topLevelAnnotator :: TopLevelAnnotator CodeOverlay
topLevelAnnotator = uncurry CodeOverlay
                <$> (singleBlockLayout *** singleBlockLayout)
                <$> topLevelContribution
