module Language.Scala.Simplifier where

import Prelude hiding (null)

import Data.Empty
import Language.Scala.Generator


class Simplify a where
  simplify :: a -> a

instance (Empty a, Simplify a) => Simplify [a] where
  simplify = foldMap go
    where
      go x = if null x' then [] else [x']
        where
          x' = simplify x

instance Simplify GeneratedCode where
  simplify (GeneratedCaseClass (CaseClass name parentName [] codeLayout)) =
      GeneratedCaseObject (CaseObject name parentName codeLayout)
  simplify x = x


-- |
-- >>> import Data.Yaml.Ordered.MyExtra
-- >>> import Raml
-- >>> import Language.Scala.Annotator
-- >>> import Language.Scala.Converter
-- >>> r <- simplify <$> generate <$> annotate mempty mempty <$> convert <$> readRaml "tests/sample.in"
-- >>> printAsYaml r
-- - - - trait:
--         name: DataType
--     - case_object:
--         name: StringType
--         parent: DataType
--     - case_object:
--         name: NumberType
--         parent: DataType
--     - case_class:
--         name: DateType
--         parent: DataType
--         fields:
--         - field:
--             name: dateFormat
--             type: String
--     - case_object:
--         name: BooleanType
--         parent: DataType
-- - - - case_class:
--         name: Field
--         fields:
--         - field:
--             name: name
--             type: String
--         - field:
--             name: dataType
--             type: DataType
instance Simplify GeneratedTree where
  simplify = GeneratedTree . simplify . unGeneratedTree
