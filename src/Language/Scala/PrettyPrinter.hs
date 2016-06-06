module Language.Scala.PrettyPrinter where

import Prelude hiding (null)

import Data.Monoid
import Text.Printf

import Data.Empty
import Data.IndentedCode
import Language.Scala.Generator


prependPrefix :: String -> CodeBlock -> CodeBlock
prependPrefix prefix = CodeBlock . go . runCodeBlock
  where
    go :: [CodeChunk] -> [CodeChunk]
    go [] = error "can't prepend a prefix to an empty block"
    go [Line s] = [Line (prefix ++ s)]
    go [Indented xs] = [Indented (prependPrefix prefix xs)]
    go (x:xs) = x : go xs

appendSuffix :: String -> CodeBlock -> CodeBlock
appendSuffix suffix = CodeBlock . go . runCodeBlock
  where
    go :: [CodeChunk] -> [CodeChunk]
    go [] = error "can't append a suffix to an empty block"
    go [Line s] = [Line (s ++ suffix)]
    go [Indented xs] = [Indented (appendSuffix suffix xs)]
    go (x:xs) = x : go xs

intercalateComma :: [CodeBlock] -> CodeBlock
intercalateComma [] = mempty
intercalateComma [x] = x
intercalateComma (x:xs) = appendSuffix "," x <> intercalateComma xs


prettyPrintField :: Field -> CodeBlock
prettyPrintField (Field name type_) = CodeBlock
    [ Line $ printf "%s: %s" name type_
    ]

prettyPrintFields :: [Field] -> CodeBlock
prettyPrintFields = intercalateComma
                  . map prettyPrintField


prettyPrintTrait :: Trait -> CodeBlock
prettyPrintTrait (Trait name codeLayout) | null codeLayout = CodeBlock
    [ Line $ printf "sealed trait %s" name
    ]
prettyPrintTrait (Trait name codeLayout) = CodeBlock
    [ Line $ printf "sealed trait %s {" name
    , Indented $ flattenLayout codeLayout
    , Line $ "}"
    ]

prettyPrintCaseObject :: CaseObject -> CodeBlock
prettyPrintCaseObject (CaseObject name Nothing codeLayout) | null codeLayout = CodeBlock
    [ Line $ printf "case object %s" name
    ]
prettyPrintCaseObject (CaseObject name (Just parentName) codeLayout) | null codeLayout = CodeBlock
    [ Line $ printf "case object %s extends %s" name parentName
    ]
prettyPrintCaseObject (CaseObject name Nothing codeLayout) = CodeBlock
    [ Line $ printf "case object %s {" name
    , Indented $ flattenLayout codeLayout
    , Line $ "}"
    ]
prettyPrintCaseObject (CaseObject name (Just parentName) codeLayout) = CodeBlock
    [ Line $ printf "case object %s extends %s {" name parentName
    , Indented $ flattenLayout codeLayout
    , Line $ "}"
    ]

prettyPrintCaseClass :: CaseClass -> CodeBlock
prettyPrintCaseClass (CaseClass name Nothing fields codeLayout) | null codeLayout = CodeBlock
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line ")"
    ]
prettyPrintCaseClass (CaseClass name (Just parentName) fields codeLayout) | null codeLayout = CodeBlock
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line $ printf ") extends %s" parentName
    ]
prettyPrintCaseClass (CaseClass name Nothing fields codeLayout) = CodeBlock
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line ") {"
    , Indented $ flattenLayout codeLayout
    , Line "}"
    ]
prettyPrintCaseClass (CaseClass name (Just parentName) fields codeLayout) = CodeBlock
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line $ printf ") extends %s {" parentName
    , Indented $ flattenLayout codeLayout
    , Line "}"
    ]

prettyPrintCompanionObject :: CompanionObject -> CodeBlock
prettyPrintCompanionObject (CompanionObject name codeLayout) = CodeBlock
    [ Line $ printf "object %s {" name
    , Indented $ flattenLayout codeLayout
    , Line $ "}"
    ]

prettyPrintGeneratedCode :: GeneratedCode -> CodeBlock
prettyPrintGeneratedCode (GeneratedTrait trait) =
    prettyPrintTrait trait
prettyPrintGeneratedCode (GeneratedCaseObject caseObject) =
    prettyPrintCaseObject caseObject
prettyPrintGeneratedCode (GeneratedCaseClass caseClass) =
    prettyPrintCaseClass caseClass
prettyPrintGeneratedCode (GeneratedCompanionObject companionObject) =
    prettyPrintCompanionObject companionObject

-- |
-- >>> import Raml
-- >>> import Language.Scala.Annotator
-- >>> import Language.Scala.Converter
-- >>> import Language.Scala.Generator
-- >>> import Language.Scala.Simplifier
-- >>> r <- flattenLayout <$> prettyPrint <$> simplify <$> generate <$> annotate mempty mempty <$> convert <$> readRaml "tests/sample.in"
-- >>> testBlock r
-- sealed trait DataType
-- case object StringType extends DataType
-- case object NumberType extends DataType
-- case class DateType(
--   dateFormat: String
-- ) extends DataType
-- case object BooleanType extends DataType
-- .
-- .
-- case class Field(
--   name: String,
--   dataType: DataType
-- )
prettyPrint :: GeneratedTree -> CodeLayout
prettyPrint = multiGroupLayout
            . map multiBlockGroup
            . (map . map . foldMap) prettyPrintGeneratedCode
            . unGeneratedTree
