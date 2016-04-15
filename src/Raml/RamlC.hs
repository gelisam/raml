module Raml.RamlC where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map, (!))

import Raml.RamlB


type RamlContext = Map String Type
type RamlCT = ReaderT RamlContext
type RamlC = RamlCT Identity

runRamlCT :: MonadIO m => FilePath -> RamlCT m a -> m a
runRamlCT filePath ramlCT = do
    ramlContext <- runRamlBT filePath $ do
      ts <- namedTypesB
      sequence $ Map.fromList [(t, lookupTypeB t) | t <- ts]
    runReaderT ramlCT ramlContext


typesC :: Monad m => RamlCT m (Map String Type)
typesC = ask

-- |
-- >>> runRamlCT "tests/sample.in" namedTypesC
-- ["Alternative","BooleanType","DataType","DateType","Field","NumberType","StringType"]
namedTypesC :: Monad m => RamlCT m [String]
namedTypesC = Map.keys <$> typesC

-- |
-- >>> runRamlCT "tests/sample.in" (lookupTypeC "BooleanType")
-- NamedType "BooleanType" (UnitType (UnitDesc {unitDiscriminator = "constructor"}))
lookupTypeC :: Monad m => String -> RamlCT m Type
lookupTypeC k = (! k) <$> typesC
