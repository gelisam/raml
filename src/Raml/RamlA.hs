module Raml.RamlA where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import qualified Data.Yaml as Yaml
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map, (!))

import Raml.Syntax


type RamlAT = ReaderT RamlFile
type RamlA = RamlAT Identity

runRamlAT :: MonadIO m => FilePath -> RamlAT m a -> m a
runRamlAT filePath ramlAT = do
    r <- liftIO $ Yaml.decodeFileEither filePath
    case r of
      Left exception -> fail (show exception)
      Right ramlFile -> runReaderT ramlAT ramlFile


ramlFileA :: Monad m => RamlAT m RamlFile
ramlFileA = ask

typesA :: Monad m => RamlAT m (Map String TypeDesc)
typesA = types <$> ramlFileA

-- |
-- >>> runRamlAT "tests/sample.in" namedTypesA
-- ["Alternative","BooleanType","DataType","DateType","Field","NumberType","StringType"]
namedTypesA :: Monad m => RamlAT m [String]
namedTypesA = Map.keys <$> typesA

-- |
-- >>> runRamlAT "tests/sample.in" (parentType <$> lookupTypeA "DataType")
-- Union [NamedType "StringType",NamedType "NumberType",NamedType "DateType",NamedType "BooleanType"]
lookupTypeA :: Monad m => String -> RamlAT m TypeDesc
lookupTypeA k = (! k) <$> typesA
