{-# LANGUAGE OverloadedStrings #-}
module Raml.Normalizer (TypeExpr(..), module Raml.Normalizer) where

import           Data.Map (Map)
import           Data.Maybe
import           Data.Yaml (ToJSON(..))

import           Raml.Common
import           Raml.Parser (TypeExpr(..), ParseTree(..))
import qualified Raml.Parser as Parser


data TypeProps = TypeProps
  { type_ :: TypeExpr -- either the property's type or the parent type
  , properties :: Maybe (Map PropertyName (OrElse TypeExpr TypeProps))
  , discriminator :: Maybe PropertyName
  , stringPattern :: Maybe Regexp
  } deriving (Show, Eq)

newtype NormalizedTree = NormalizedTree
  { unNormalizedTree :: Map TypeName (OrElse TypeExpr TypeProps)
  } deriving (Show, Eq)


instance ToJSON TypeProps where
  toJSON t = object [ "type" .=! type_ t
                    , "properties" .=? properties t
                    , "discriminator" .=? discriminator t
                    , "pattern" .=? stringPattern t
                    ]

instance ToJSON NormalizedTree where
  toJSON t = object [ "types" .=! unNormalizedTree t
                    ]


normalizePropertyType :: Maybe (OrElse TypeExpr Parser.TypeProps)
                      -> OrElse TypeExpr TypeProps
normalizePropertyType Nothing = OrElse (Left String)
normalizePropertyType (Just (OrElse (Left  x))) = OrElse (Left x)
normalizePropertyType (Just (OrElse (Right y))) = OrElse (Right (normalizeTypeProps y))

normalizeTypeProps :: Parser.TypeProps -> TypeProps
normalizeTypeProps p = TypeProps
                     { type_ = fromMaybe defaultType
                             $ Parser.type_ p
                     , properties = (fmap . fmap) normalizePropertyType
                                  $ Parser.properties p
                     , discriminator = Parser.discriminator p
                     , stringPattern = Parser.stringPattern p
                     }
  where
    defaultType :: TypeExpr
    defaultType = case Parser.properties p of
      Nothing -> String
      Just _ -> Object

-- |
-- >>> r <- normalize <$> Parser.parse <$> readYaml "tests/sample.in"
-- >>> printAsYaml r
-- types:
--   BooleanType: Alternative
--   DateType:
--     type: Alternative
--     properties:
--       dateFormat:
--         pattern: ! '[YMD]+[-\.][YMD]+[-\.\/][YMD]+'
--         type: string
--   Alternative:
--     discriminator: constructor
--     type: object
--   Field:
--     type: object
--     properties:
--       name: string
--       dataType: DataType
--   NumberType: Alternative
--   StringType: Alternative
--   DataType: (StringType | NumberType | DateType | BooleanType)
normalize :: ParseTree -> NormalizedTree
normalize = NormalizedTree . (fmap . fmap) normalizeTypeProps . unParseTree
