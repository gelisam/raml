{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}
module Raml.Common where

import Control.Applicative
import Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Yaml (FromJSON(..), ToJSON(..))
import qualified Data.Yaml as Yaml


type TypeName = String
type PropertyName = String
type Regexp = String

-- Either's json encoding is "{Left: ...}",
-- OrElse's json encoding is "..."
newtype OrElse a b = OrElse
  { unOrElse :: Either a b
  } deriving (Show, Eq)


pattern YamlString s <- Yaml.String (Text.unpack -> s) where
  YamlString s = Yaml.String (Text.pack s)

-- Yaml.object ["foo" .= Nothing] is {"foo": null},
-- object ["foo" .=? Nothing] is {}.
object :: [Maybe (Text, Yaml.Value)] -> Yaml.Value
object = Yaml.object . catMaybes

(.=?) :: ToJSON a => Text -> Maybe a -> Maybe (Text, Yaml.Value)
key .=? value = do
    v <- value
    return (key, toJSON v)


instance (FromJSON a, FromJSON b) => FromJSON (OrElse a b) where
  parseJSON v = OrElse <$> ( Left  <$> parseJSON v
                         <|> Right <$> parseJSON v
                           )

instance (ToJSON a, ToJSON b) => ToJSON (OrElse a b) where
  toJSON (OrElse (Left  x)) = toJSON x
  toJSON (OrElse (Right y)) = toJSON y
