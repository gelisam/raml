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
--         name: StringType
--         parent: DataType
--     - case_object:
--         name: NumberType
--         parent: DataType
--     - case_class:
--         name: DateType
--         parent: DataType
--         parameters:
--         - field:
--             name: dateFormat
--             type: String
--         requirements:
--         - - dateFormat match {
--           - - case DateType.DateFormatPattern() => true
--             - case _ => false
--           - ! '}'
--     - case_object:
--         name: BooleanType
--         parent: DataType
--   - - companion_object:
--         name: DateType
--         vals:
--         - val:
--             name: DateFormatPattern
--             value: ! '"[YMD]+[-\\.][YMD]+[-\\.\\/][YMD]+".r'
-- - - - case_class:
--         name: Field
--         parameters:
--         - field:
--             name: name
--             type: String
--         - field:
--             name: dataType
--             type: DataType
instance Simplify GeneratedTree where
  simplify = GeneratedTree . simplify . unGeneratedTree
