module Raml.ScalaName where

import qualified Data.String.MyExtra as String
import Raml.Common


data ScalaName
  = ScalaName String
  | PrefixedName String ScalaName
  | QualifiedName TypeName ScalaName
  deriving (Show, Eq)

nameToString :: ScalaName -> String
nameToString (ScalaName s) = s
nameToString (PrefixedName prefix s) = prefix String.+++ nameToString s
nameToString (QualifiedName qualifier s) = qualifier String..++ nameToString s

infixr 4 +++
(+++) :: String -> ScalaName -> ScalaName
(+++) = PrefixedName

infixr 4 .++
(.++) :: String -> ScalaName -> ScalaName
(.++) = QualifiedName


-- |
-- >>> nameToString $ capitalize $ "Foo" .++ "bar" +++ ScalaName "baz"
-- "Foo.BarBaz"
capitalize :: ScalaName -> ScalaName
capitalize (ScalaName s) = ScalaName (String.capitalize s)
capitalize (PrefixedName prefix s) = PrefixedName (String.capitalize prefix) s
capitalize (QualifiedName qualifier s) = QualifiedName qualifier (capitalize s)
