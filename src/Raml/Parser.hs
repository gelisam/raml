{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}
module Raml.Parser where

import Control.Applicative
import qualified Data.Aeson as Json
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Text as Text
import           Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import qualified Data.Yaml as Yaml
import Extra
import Text.Printf


type TypeName = String
type PropertyName = String
type Regexp = String

-- Either's json encoding is "{Left: ...}",
-- OrElse's json encoding is "..."
newtype OrElse a b = OrElse
  { unOrElse :: Either a b
  } deriving (Show, Eq)

data TypeExpr
  = Object
  | String
  | Ref TypeName
  | Union [TypeExpr]
  deriving (Show, Eq)

data TypeProps = TypeProps
  { type_ :: Maybe TypeExpr -- either the property's type or the parent type
  , properties :: Maybe (Map PropertyName (Maybe (OrElse TypeExpr TypeProps)))
  , discriminator :: Maybe PropertyName
  , stringPattern :: Maybe Regexp
  } deriving (Show, Eq)

newtype ParseTree = ParseTree
  { unParseTree :: Map TypeName (OrElse TypeExpr TypeProps)
  } deriving (Show, Eq)


pattern YamlString s <- Yaml.String (Text.unpack -> s) where
  YamlString s = Yaml.String (Text.pack s)


instance (FromJSON a, FromJSON b) => FromJSON (OrElse a b) where
  parseJSON v = OrElse <$> ( Left  <$> parseJSON v
                         <|> Right <$> parseJSON v
                           )

instance (ToJSON a, ToJSON b) => ToJSON (OrElse a b) where
  toJSON (OrElse (Left  x)) = toJSON x
  toJSON (OrElse (Right y)) = toJSON y

instance FromJSON TypeExpr where
  parseJSON (YamlString s) = case wordsBy (`elem` [' ', '|']) s of
      [] -> fail $ printf "expected type expression, got %s" (show (YamlString s))
      ["object"] -> return Object
      ["string"] -> return String
      [x] -> return $ Ref x
      xs -> Union <$> mapM (parseJSON . YamlString) xs
  parseJSON v = fail $ printf "expected type expression, got %s" (show v)

instance ToJSON TypeExpr where
  toJSON = YamlString . toString
    where
      toString :: TypeExpr -> String
      toString Object = "object"
      toString String = "string"
      toString (Ref x) = x
      toString (Union xs) = printf "(%s)"
                          $ List.intercalate " | "
                          $ map toString xs

instance FromJSON TypeProps where
  parseJSON (Yaml.Object o) = TypeProps
                          <$> o .:? "type"
                          <*> o .:? "properties"
                          <*> o .:? "discriminator"
                          <*> o .:? "pattern"
  parseJSON v = fail $ printf "expected type properties, got %s" (show v)

instance ToJSON TypeProps where
  toJSON t = Yaml.object [ "type" .= type_ t
                         , "properties" .= properties t
                         , "discriminator" .= discriminator t
                         , "pattern" .= stringPattern t
                         ]

instance FromJSON ParseTree where
  parseJSON (Yaml.Object o) = ParseTree
                          <$> o .: "types"
  parseJSON v = fail $ printf "expected type list, got %s" (show v)

instance ToJSON ParseTree where
  toJSON t = Yaml.object [ "types" .= unParseTree t
                         ]


readYaml :: FilePath -> IO Yaml.Value
readYaml file = do
    r <- Yaml.decodeFileEither file
    case r of
      Left err -> error (Yaml.prettyPrintParseException err)
      Right x -> return x

-- |
-- >>> import qualified Data.ByteString.Char8 as B
-- >>> r <- parse <$> readYaml "tests/sample.in"
-- >>> B.putStrLn $ Yaml.encode r
-- types:
--   BooleanType: Alternative
--   DateType:
--     pattern: null
--     discriminator: null
--     type: Alternative
--     properties:
--       dateFormat:
--         pattern: ! '[YMD]+[-\.][YMD]+[-\.\/][YMD]+'
--         discriminator: null
--         type: string
--         properties: null
--   Alternative:
--     pattern: null
--     discriminator: constructor
--     type: object
--     properties: null
--   Field:
--     pattern: null
--     discriminator: null
--     type: null
--     properties:
--       name: null
--       dataType: DataType
--   NumberType: Alternative
--   StringType: Alternative
--   DataType: (StringType | NumberType | DateType | BooleanType)
parse :: Yaml.Value -> ParseTree
parse v = case Json.fromJSON v of
    Json.Error err -> error err
    Json.Success x -> x
