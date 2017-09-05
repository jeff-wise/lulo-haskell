
{-| Lulo Document

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Document where


import Lulo.Schema.Types

import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Yaml as Yaml (Value (..))



data Doc = 
    DocDict (HashMap Text Doc)
  | DocList [Doc]
  | DocText Text
  | DocNumber Scientific
  | DocBoolean Bool



type DocParser a = Either DocParseError a





data DocParseError = 
    UnexpectedYamlType UnexpectedYamlTypeError
  | UnknownPrimitiveType DocPath
  | TypeDoesNotExist CustomTypeName DocPath
  | FieldMissing FieldName DocPath
  | SumTypeMissingType CustomTypeName DocPath
  | SumTypeDoesNotHaveCase CustomTypeName DocPath
  | SumTypeMissingValue CustomTypeName CustomTypeName DocPath


data UnexpectedYamlTypeError = UnexpectedYamlTypeError
  { expectedYamlType :: YamlType
  , actualYamlType :: YamlType
  , unexpectedYamlTypePath :: DocPath
  }


data YamlType = 
    YamlObject 
  | YamlArray 
  | YamlString 
  | YamlNumber 
  | YamlBool 
  | YamlNull


yamlType :: Yaml.Value -> YamlType
yamlType (Yaml.Object _) = YamlObject
yamlType (Yaml.Array  _) = YamlArray
yamlType (Yaml.String _) = YamlString
yamlType (Yaml.Number _) = YamlNumber
yamlType (Yaml.Bool   _) = YamlBool
yamlType Yaml.Null       = YamlNull


newtype DocCase = DocCase
  { getDocCase :: Text }
  deriving (Eq, Show)




newtype DocPath = DocPath
  { getDocPath :: [DocNode] }
  deriving (Eq)


data DocNode = 
    DocKeyNode Text
  | DocIndexNode Int
  deriving (Eq)

