module Language.Scala.Name where

import qualified Data.String.MyExtra as String
import Raml.Common


data Name
  = Name String
  | PrefixedName String Name
  | QualifiedName TypeName Name
  deriving (Show, Eq)

nameToString :: Name -> String
nameToString (Name s) = s
nameToString (PrefixedName prefix s) = prefix String.+++ nameToString s
nameToString (QualifiedName qualifier s) = qualifier String..++ nameToString s

infixr 4 +++
(+++) :: String -> Name -> Name
(+++) = PrefixedName

infixr 4 .++
(.++) :: String -> Name -> Name
(.++) = QualifiedName


-- |
-- >>> nameToString $ capitalize $ "Foo" .++ "bar" +++ Name "baz"
-- "Foo.BarBaz"
capitalize :: Name -> Name
capitalize (Name s) = Name (String.capitalize s)
capitalize (PrefixedName prefix s) = PrefixedName (String.capitalize prefix) s
capitalize (QualifiedName qualifier s) = QualifiedName qualifier (capitalize s)


type CompanionNamer = PropertyName -> String -> Name

qualifiedCompanionNamer :: TypeName -> CompanionNamer
qualifiedCompanionNamer companionName fieldName varName =
    companionName .++ fieldName +++ Name varName

unqualifiedCompanionNamer :: CompanionNamer
unqualifiedCompanionNamer fieldName varName =
    fieldName +++ Name varName
