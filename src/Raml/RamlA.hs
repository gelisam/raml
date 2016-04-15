module Raml.RamlA where

import qualified Data.Yaml as Yaml
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map, (!))

import Raml.Syntax


type RamlA a = RamlFile -> a

runRamlA :: FilePath -> RamlA a -> IO a
runRamlA filePath f = do
    r <- Yaml.decodeFileEither filePath
    case r of
      Left exception -> fail (show exception)
      Right ramlFile -> return (f ramlFile)


getNamedTypes :: RamlA [String]
getNamedTypes = Map.keys . types

lookupType :: String -> RamlA TypeDesc
lookupType k r = types r ! k
