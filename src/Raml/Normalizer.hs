{-# LANGUAGE OverloadedStrings #-}
module Raml.Normalizer (TypeExpr(..), module Raml.Normalizer) where

import           Data.Map (Map)
import           Data.Maybe
import           Data.Yaml.Ordered (ToJSON(..))

import Data.Yaml.Ordered.MyExtra
import Raml.Common
import           Raml.Parser (TypeExpr(..), ParseTree(..))
import qualified Raml.Parser as Parser


data TypeProps = TypeProps
  { parentType :: TypeExpr
  , properties :: Maybe (Map PropertyName TypeProps)
  , discriminator :: Maybe Discriminator
  , stringPattern :: Maybe Regexp
  } deriving (Show, Eq)


type SymbolTable = Map TypeName TypeProps

newtype NormalizedTree = NormalizedTree
  { unNormalizedTree :: SymbolTable
  } deriving (Show, Eq)


instance ToJSON TypeProps where
  toJSON t = object [ "type" .=! parentType t
                    , "properties" .=? properties t
                    , "discriminator" .=? discriminator t
                    , "pattern" .=? stringPattern t
                    ]

instance ToJSON NormalizedTree where
  toJSON t = object [ "types" .=! unNormalizedTree t
                    ]


normalizePropertyType :: Maybe (OrElse TypeExpr Parser.TypeProps)
                      -> TypeProps
normalizePropertyType Nothing = TypeProps
                              { parentType = String
                              , properties = Nothing
                              , discriminator = Nothing
                              , stringPattern = Nothing
                              }
normalizePropertyType (Just (OrElse (Left x))) = TypeProps
                                               { parentType = x
                                               , properties = Nothing
                                               , discriminator = Nothing
                                               , stringPattern = Nothing
                                               }
normalizePropertyType (Just (OrElse (Right y))) = normalizeTypeProps y

normalizeTypeProps :: Parser.TypeProps -> TypeProps
normalizeTypeProps p = TypeProps
                     { parentType = fromMaybe defaultType
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
-- >>> import Raml.Parser
-- >>> r <- normalize <$> parse <$> readYaml "tests/sample.in"
-- >>> printAsYaml r
-- types:
--   Alternative:
--     type: object
--     discriminator: constructor
--   BooleanType:
--     type: Alternative
--   DataType:
--     type: (StringType | NumberType | DateType | BooleanType)
--   DateType:
--     type: Alternative
--     properties:
--       dateFormat:
--         type: string
--         pattern: ! '[YMD]+[-\.][YMD]+[-\.\/][YMD]+'
--   Field:
--     type: object
--     properties:
--       dataType:
--         type: DataType
--       name:
--         type: string
--   NumberType:
--     type: Alternative
--   StringType:
--     type: Alternative
normalize :: ParseTree -> NormalizedTree
normalize = NormalizedTree . fmap (normalizePropertyType . Just) . unParseTree
