module Raml.Simplifier where

import Prelude hiding (null)

import Data.Empty
import Raml.Generator


class Simplify a where
  simplify :: a -> a

instance (Empty a, Simplify a) => Simplify [a] where
  simplify = foldMap go
    where
      go x = if null x' then [] else [x']
        where
          x' = simplify x

instance Simplify GeneratedCode where
  simplify (GeneratedCaseClass (CaseClass name parentName [] [])) =
      GeneratedCaseObject (CaseObject name parentName)
  simplify x = x


-- |
-- >>> import Data.Yaml.Ordered.MyExtra
-- >>> import Raml.Parser
-- >>> import Raml.Normalizer
-- >>> import Raml.Classifier
-- >>> import Raml.Analyzer
-- >>> r <- simplify <$> generate <$> analyze <$> classify <$> normalize <$> parse <$> readYaml "tests/sample.in"
-- >>> printAsYaml r
-- - - - trait:
--         name: DataType
--     - case_object:
--         parent: DataType
--         name: BooleanType
--     - case_class:
--         parent: DataType
--         requirements:
--         - - dateFormat match {
--           - - case DateType.DateFormatPattern() => true
--             - case _ => false
--           - ! '}'
--         name: DateType
--         parameters:
--         - field:
--             name: dateFormat
--             type: String
--     - case_object:
--         parent: DataType
--         name: NumberType
--     - case_object:
--         parent: DataType
--         name: StringType
--   - - companion_object:
--         name: DateType
--         vals:
--         - val:
--             value: ! '"[YMD]+[-\\.][YMD]+[-\\.\\/][YMD]+".r'
--             name: DateFormatPattern
-- - - - case_class:
--         name: Field
--         parameters:
--         - field:
--             name: dataType
--             type: DataType
--         - field:
--             name: name
--             type: String
instance Simplify GeneratedTree where
  simplify = GeneratedTree . simplify . unGeneratedTree
