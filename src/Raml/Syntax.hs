{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Raml.Syntax where

import Control.Applicative
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Yaml (Value(..), Parser, FromJSON(..), (.:), (.:?), (.!=))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import Extra
import Text.Printf

-- $setup
-- >>> import qualified Data.ByteString.Char8 as ByteString
-- >>> import qualified Data.Yaml as Yaml


data Raml = Raml
  { types :: Map String TypeDesc
  } deriving (Show, Eq)

data TypeDesc = TypeDesc
  { parentType :: TypeExpr
  , properties :: Map String PropertyDesc
  , discriminator :: Maybe String
  , enum :: [String]
  , stringPattern :: Maybe String
  } deriving (Show, Eq)

data TypeExpr
  = PrimitiveType PrimitiveType
  | NamedType String
  | Union [TypeExpr]
  deriving (Show, Eq)

data PrimitiveType
  = PrimitiveObject
  | PrimitiveString
  deriving (Show, Eq)

data PropertyDesc = PropertyDesc
  { required :: Bool
  , type_ :: TypeExpr
  , default_ :: Maybe Value
  } deriving (Show, Eq)


instance FromJSON Raml where
  parseJSON (Object o) = Raml
                     <$> o .: "types"
  parseJSON v = fail $ printf "expected RAML file, got %s" (show v)

instance FromJSON TypeDesc where
  parseJSON v = parseTypeDesc v
            <|> parseObjectDesc v
            <|> parseTypeExpr v
    where
      parseTypeDesc :: Value -> Parser TypeDesc
      parseTypeDesc (Object o) = TypeDesc
                             <$> o .:  "type"
                             <*> o .:? "properties" .!= Map.empty
                             <*> o .:? "discriminator" 
                             <*> o .:? "enum" .!= []
                             <*> o .:? "pattern"
      parseTypeDesc v' = fail $ printf "expected type definition, got %s" (show v')
      
      parseObjectDesc :: Value -> Parser TypeDesc
      parseObjectDesc (Object o) = TypeDesc
                               <$> pure (PrimitiveType PrimitiveObject)
                               <*> o .: "properties"
                               <*> o .:? "discriminator" 
                               <*> o .:? "enum" .!= []
                               <*> o .:? "pattern"
      parseObjectDesc v' = fail $ printf "expected type definition, got %s" (show v')
      
      parseTypeExpr :: Value -> Parser TypeDesc
      parseTypeExpr v' = TypeDesc
                     <$> parseJSON v'
                     <*> pure Map.empty
                     <*> pure Nothing
                     <*> pure []
                     <*> pure Nothing

-- |
-- >>> Yaml.decode (ByteString.pack "type:") :: Maybe (Map String TypeExpr)
-- Just (fromList [("type",PrimitiveType PrimitiveString)])
-- >>> Yaml.decode (ByteString.pack "type: object") :: Maybe (Map String TypeExpr)
-- Just (fromList [("type",PrimitiveType PrimitiveObject)])
-- >>> Yaml.decode (ByteString.pack "type: object|string") :: Maybe (Map String TypeExpr)
-- Just (fromList [("type",Union [PrimitiveType PrimitiveObject,PrimitiveType PrimitiveString])])
-- >>> Yaml.decode (ByteString.pack "type: MyNumber") :: Maybe (Map String TypeExpr)
-- Just (fromList [("type",NamedType "MyNumber")])
-- >>> Yaml.decode (ByteString.pack "type: object|MyNumber|MyString") :: Maybe (Map String TypeExpr)
-- Just (fromList [("type",Union [PrimitiveType PrimitiveObject,NamedType "MyNumber",NamedType "MyString"])])
instance FromJSON TypeExpr where
  parseJSON Null = return $ PrimitiveType PrimitiveString  -- default
  parseJSON (String s) = case components s of
      [] -> fail $ printf "expected type expression, got %s" (show (String s))
      [x] -> parsePrimitiveType x <|> parseNamedType
      xs -> Union <$> mapM parseJSON xs
    where
      components :: Text -> [Value]
      components = map (String . Text.pack)
                 . wordsBy (`elem` [' ', '|'])
                 . Text.unpack
      
      parsePrimitiveType :: Value -> Parser TypeExpr
      parsePrimitiveType x = PrimitiveType <$> parseJSON x
      
      parseNamedType :: Parser TypeExpr
      parseNamedType = return $ NamedType (Text.unpack s)
  parseJSON v = fail $ printf "expected type expression, got %s" (show v)

-- |
-- >>> Yaml.decode (ByteString.pack "type: object") :: Maybe (Map String PrimitiveType)
-- Just (fromList [("type",PrimitiveObject)])
instance FromJSON PrimitiveType where
  parseJSON (String "object") = return PrimitiveObject
  parseJSON (String "string") = return PrimitiveString
  parseJSON v = fail $ printf "expected type expression, got %s" (show v)

instance FromJSON PropertyDesc where
  parseJSON v = parseTypeDesc v
            <|> parsePropertyDesc v
    where
      parseTypeDesc :: Value -> Parser PropertyDesc
      parseTypeDesc v' = PropertyDesc
                     <$> pure True
                     <*> parseJSON v'
                     <*> pure Nothing
      
      parsePropertyDesc :: Value -> Parser PropertyDesc
      parsePropertyDesc (Object o) = PropertyDesc
                                 <$> o .:? "required" .!= True
                                 <*> o .:  "type"
                                 <*> o .:? "default" .!= Nothing
      parsePropertyDesc v' = fail $ printf "expected property description, got %s" (show v')
