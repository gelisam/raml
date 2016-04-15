module Raml.RamlD where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Functor.Identity
import           Data.Yaml (Value)
import Text.Printf

import qualified Raml.RamlB as RamlB
import Raml.RamlC
import qualified Raml.Syntax as Syntax
import           Raml.Syntax (PrimitiveType(..))


data TypeDefinition
  = SumType SumDesc
  | ConcreteType ConcreteDesc
  deriving (Show, Eq)

data SumDesc = SumDesc
  { traitName :: String
  , alternatives :: [ConcreteDesc]
  } deriving (Show, Eq)

data ConcreteDesc
  = CaseObject ObjectDesc
  | CaseClass ClassDesc
  deriving (Show, Eq)

data ObjectDesc = ObjectDesc
  { objectName :: String
  } deriving (Show, Eq)

data ClassDesc = ClassDesc
  { className :: String
  , params :: [ParamDesc]
  } deriving (Show, Eq)

data ParamDesc = ParamDesc
  { paramName :: String
  , paramType :: ParamType
  , default_ :: Maybe Value
  } deriving (Show, Eq)

data ParamType
  = PrimitiveType PrimitiveType
  | NamedType String
  | MaybeType ParamType
  deriving (Show, Eq)


type RamlDT m = RamlCT (EitherT String m)
type RamlD = RamlDT Identity

runRamlDT :: MonadIO m => FilePath -> RamlDT m a -> m a
runRamlDT filePath ramlDT = do
    r <- runEitherT $ runRamlCT filePath ramlDT
    case r of
      Left err -> fail (show err)
      Right x -> return x


-- |
-- >>> runRamlDT "tests/sample.in" namedTypesD
-- ["Alternative","BooleanType","DataType","DateType","Field","NumberType","StringType"]
namedTypesD :: Monad m => RamlDT m [String]
namedTypesD = namedTypesC

-- |
-- >>> runRamlDT "tests/sample.in" (lookupTypeD "BooleanType")
-- NamedType "BooleanType" (UnitType (UnitDesc {unitDiscriminator = "constructor"}))
lookupTypeD :: Monad m => String -> RamlDT m TypeDefinition
lookupTypeD s = do
    typeDesc <- lookupTypeC s
    typeDefinitionD s typeDesc


namedD :: Monad m
       => (String -> RamlB.Type -> RamlDT m b)
       -> RamlB.Type -> RamlDT m b
namedD f (RamlB.NamedType s t) = f s t
namedD _ t = fail $ printf "expected named type, got %s" (show t)

typeDefinitionD :: Monad m => String -> RamlB.Type -> RamlDT m TypeDefinition
typeDefinitionD name (RamlB.NamedType s t) | name == s = typeDefinitionD name t
                                           | otherwise =
    fail $ printf "conflicting type names: %s" (show [name, s])
typeDefinitionD name (RamlB.UnionType ts) =
    SumType <$> (SumDesc name <$> traverse (namedD concreteDescD) ts)
typeDefinitionD name t =
    ConcreteType <$> concreteDescD name t

concreteDescD :: Monad m => String -> RamlB.Type -> RamlDT m ConcreteDesc
concreteDescD = undefined
