module Language.Scala.PrettyPrinter where

import Data.Monoid
import Text.Printf

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


prettyPrintRequirement :: CodeChunk -> CodeBlock
prettyPrintRequirement (Line s) = CodeBlock
    [ Line $ printf "require(%s)" s
    ]
prettyPrintRequirement (Indented ss) = CodeBlock
    [ Line "require("
    , Indented ss
    , Line ")"
    ]

prettyPrintRequirements :: [CodeChunk] -> CodeBlock
prettyPrintRequirements = flattenGroup
                        . multiBlockGroup
                        . map prettyPrintRequirement


prettyPrintVal :: Val -> CodeBlock
prettyPrintVal (Val name (Line value)) = CodeBlock
    [ Line $ printf "val %s = %s" name value
    ]
prettyPrintVal (Val name (Indented xs)) = CodeBlock
    [ Line $ printf "val %s =" name
    , Indented xs
    ]

prettyPrintVals :: [Val] -> CodeBlock
prettyPrintVals = foldMap prettyPrintVal


prettyPrintTrait :: Trait -> CodeBlock
prettyPrintTrait (Trait name) = CodeBlock
    [ Line $ printf "sealed trait %s" name
    ]

prettyPrintCaseObject :: CaseObject -> CodeBlock
prettyPrintCaseObject (CaseObject name Nothing) = CodeBlock
    [ Line $ printf "case object %s" name
    ]
prettyPrintCaseObject (CaseObject name (Just parentName)) = CodeBlock
    [ Line $ printf "case object %s extends %s" name parentName
    ]

prettyPrintCaseClass :: CaseClass -> CodeBlock
prettyPrintCaseClass (CaseClass name Nothing fields []) = CodeBlock
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line ")"
    ]
prettyPrintCaseClass (CaseClass name (Just parentName) fields []) = CodeBlock
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line $ printf ") extends %s" parentName
    ]
prettyPrintCaseClass (CaseClass name Nothing fields reqs) = CodeBlock
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line ") {"
    , Indented $ prettyPrintRequirements reqs
    , Line "}"
    ]
prettyPrintCaseClass (CaseClass name (Just parentName) fields reqs) = CodeBlock
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line $ printf ") extends %s {" parentName
    , Indented $ prettyPrintRequirements reqs
    , Line "}"
    ]

prettyPrintCompanionObject :: CompanionObject -> CodeBlock
prettyPrintCompanionObject (CompanionObject name vals) = CodeBlock
    [ Line $ printf "object %s {" name
    , Indented $ prettyPrintVals vals
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
-- >>> import Language.Scala.Generator
-- >>> import Language.Scala.Simplifier
-- >>> r <- flattenLayout <$> prettyPrint <$> simplify <$> generate <$> readRaml "tests/sample.in"
-- >>> testBlock r
-- sealed trait DataType
-- case object StringType extends DataType
-- case object NumberType extends DataType
-- case class DateType(
--   dateFormat: String
-- ) extends DataType {
--   require(
--     dateFormat match {
--       case DateType.DateFormatPattern() => true
--       case _ => false
--     }
--   )
-- }
-- case object BooleanType extends DataType
-- .
-- object DateType {
--   val DateFormatPattern = "[YMD]+[-\\.][YMD]+[-\\.\\/][YMD]+".r
-- }
-- .
-- .
-- case class Field(
--   name: String,
--   dataType: DataType
-- )
prettyPrint :: GeneratedTree -> CodeLayout
prettyPrint = CodeLayout
            . map CodeGroup
            . (map . map . foldMap) prettyPrintGeneratedCode
            . unGeneratedTree
