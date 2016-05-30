module Language.Scala.PrettyPrinter where

import Data.List
import Text.Printf

import Data.IndentedCode
import Language.Scala.Generator


prependPrefix :: String -> IndentedCode -> IndentedCode
prependPrefix prefix = go
  where
    go :: IndentedCode -> IndentedCode
    go [] = error "can't prepend a prefix to an empty block"
    go [Line s] = [Line (prefix ++ s)]
    go [Indented xs] = [Indented (go xs)]
    go (x:xs) = x : go xs

appendSuffix :: String -> IndentedCode -> IndentedCode
appendSuffix suffix = go
  where
    go :: IndentedCode -> IndentedCode
    go [] = error "can't append a suffix to an empty block"
    go [Line s] = [Line (s ++ suffix)]
    go [Indented xs] = [Indented (go xs)]
    go (x:xs) = x : go xs

intercalateComma :: [IndentedCode] -> IndentedCode
intercalateComma [] = []
intercalateComma [x] = x
intercalateComma (x:xs) = appendSuffix "," x ++ intercalateComma xs


prettyPrintField :: Field -> IndentedCode
prettyPrintField (Field name type_) =
    [ Line $ printf "%s: %s" name type_
    ]

prettyPrintFields :: [Field] -> IndentedCode
prettyPrintFields = intercalateComma
                  . map prettyPrintField


prettyPrintRequirement :: CodeChunk -> IndentedCode
prettyPrintRequirement (Line s) =
    [ Line $ printf "require(%s)" s
    ]
prettyPrintRequirement (Indented ss) =
    [ Line "require("
    , Indented ss
    , Line ")"
    ]

prettyPrintRequirements :: [CodeChunk] -> IndentedCode
prettyPrintRequirements = intercalate singleBlank
                        . map prettyPrintRequirement


prettyPrintVal :: Val -> IndentedCode
prettyPrintVal (Val name (Line value)) =
    [ Line $ printf "val %s = %s" name value
    ]
prettyPrintVal (Val name (Indented xs)) =
    [ Line $ printf "val %s =" name
    , Indented xs
    ]

prettyPrintVals :: [Val] -> IndentedCode
prettyPrintVals = concatMap prettyPrintVal


prettyPrintTrait :: Trait -> IndentedCode
prettyPrintTrait (Trait name) =
    [ Line $ printf "sealed trait %s" name
    ]

prettyPrintCaseObject :: CaseObject -> IndentedCode
prettyPrintCaseObject (CaseObject name Nothing) =
    [ Line $ printf "case object %s" name
    ]
prettyPrintCaseObject (CaseObject name (Just parentName)) =
    [ Line $ printf "case object %s extends %s" name parentName
    ]

prettyPrintCaseClass :: CaseClass -> IndentedCode
prettyPrintCaseClass (CaseClass name Nothing fields []) =
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line ")"
    ]
prettyPrintCaseClass (CaseClass name (Just parentName) fields []) =
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line $ printf ") extends %s" parentName
    ]
prettyPrintCaseClass (CaseClass name Nothing fields reqs) =
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line ") {"
    , Indented $ prettyPrintRequirements reqs
    , Line "}"
    ]
prettyPrintCaseClass (CaseClass name (Just parentName) fields reqs) =
    [ Line $ printf "case class %s(" name
    , Indented $ prettyPrintFields fields
    , Line $ printf ") extends %s {" parentName
    , Indented $ prettyPrintRequirements reqs
    , Line "}"
    ]

prettyPrintCompanionObject :: CompanionObject -> IndentedCode
prettyPrintCompanionObject (CompanionObject name vals) =
    [ Line $ printf "object %s {" name
    , Indented $ prettyPrintVals vals
    , Line $ "}"
    ]

prettyPrintGeneratedCode :: GeneratedCode -> IndentedCode
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
-- >>> r <- prettyPrint <$> simplify <$> generate <$> readRaml "tests/sample.in"
-- >>> printIndented r
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
-- <BLANKLINE>
-- object DateType {
--   val DateFormatPattern = "[YMD]+[-\\.][YMD]+[-\\.\\/][YMD]+".r
-- }
-- <BLANKLINE>
-- <BLANKLINE>
-- case class Field(
--   name: String,
--   dataType: DataType
-- )
prettyPrint :: GeneratedTree -> [[IndentedCode]]
prettyPrint = (map . map) concat
            . (map . map . map) prettyPrintGeneratedCode
            . unGeneratedTree
