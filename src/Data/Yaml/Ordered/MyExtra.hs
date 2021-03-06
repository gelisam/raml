{-# LANGUAGE DeriveFunctor, OverloadedStrings, PatternSynonyms, ViewPatterns #-}
module Data.Yaml.Ordered.MyExtra where

import Prelude hiding (null)

import Control.Applicative
import Data.Maybe
import qualified Data.ByteString.Char8 as ByteString
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Yaml.Ordered (FromJSON(..), ToJSON(..))
import qualified Data.Yaml.Ordered as Yaml

import Data.Empty


-- Either's json encoding is "{Left: ...}",
-- OrElse's json encoding is "..."
newtype OrElse a b = OrElse
  { unOrElse :: Either a b
  } deriving (Show, Eq, Functor)


pattern YamlString s <- Yaml.String (Text.unpack -> s) where
  YamlString s = Yaml.String (Text.pack s)

-- Yaml.object ["foo" .= Nothing] is {"foo": null},
-- object ["foo" .=? Nothing] is {}.
object :: [Maybe (Text, Yaml.Value)] -> Yaml.Value
object = Yaml.object . catMaybes

(.=?) :: (ToJSON a, Empty a) => Text -> a -> Maybe (Text, Yaml.Value)
key .=? value = if null value then Nothing else Just (key, toJSON value)

(.=!) :: ToJSON a => Text -> a -> Maybe (Text, Yaml.Value)
key .=! value = return (key, toJSON value)


instance (FromJSON a, FromJSON b) => FromJSON (OrElse a b) where
  parseJSON v = OrElse <$> ( Left  <$> parseJSON v
                         <|> Right <$> parseJSON v
                           )

instance (ToJSON a, ToJSON b) => ToJSON (OrElse a b) where
  toJSON (OrElse (Left  x)) = toJSON x
  toJSON (OrElse (Right y)) = toJSON y


readYaml :: FilePath -> IO Yaml.Value
readYaml file = do
    r <- Yaml.decodeFileEither file
    case r of
      Left err -> error (Yaml.prettyPrintParseException err)
      Right x -> return x

printAsYaml :: ToJSON a => a -> IO ()
printAsYaml = ByteString.putStr . Yaml.encode
