
{-| Lulo Document Parsers

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Doc.Parser where


import Lulo.Doc.Types
import Lulo.Value

import Data.Monoid
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Yaml as Yaml (Value (..))

import qualified Data.HashMap.Strict as HM (lookup)



--------------------------------------------------------------------------------
-- VALUE PARSERS
--------------------------------------------------------------------------------

-- Value Parsers > Dictionary
--------------------------------------------------------------------------------

-- Value Parsers > Dictionary > Doc
--------------------------------------------------------------------------------

atParser :: Text -> DictDoc -> ValueParser Doc
atParser key (DictDoc hm _ path) = 
  case HM.lookup key hm of
    Just doc -> return doc
    Nothing  -> Left $ ValueParseErrorMissingKey $ MissingKeyError key path


maybeAtParser :: Text -> DictDoc -> ValueParser (Maybe Doc)
maybeAtParser key (DictDoc hm _ _) = 
  case HM.lookup key hm of
    Just doc -> return $ Just doc
    Nothing  -> return Nothing


-- Value Parsers > Dictionary > List
--------------------------------------------------------------------------------

atListParser :: Text -> DictDoc -> ValueParser ListDoc
atListParser key (DictDoc hm _ path) = 
  case HM.lookup key hm of
    Just doc -> 
      case doc of
        DocList listDoc -> return listDoc
        _               -> Left $ ValueParseErrorUnexpectedType $
                             UnexpectedTypeError DocListType (docType doc) path
    Nothing  -> Left $ ValueParseErrorMissingKey $ MissingKeyError key path


atMaybeListParser :: Text -> DictDoc -> ValueParser ListDoc
atMaybeListParser key (DictDoc hm _ path) = 
  case HM.lookup key hm of
    Just doc -> 
      case doc of
        DocList listDoc -> return listDoc
        _               -> Left $ ValueParseErrorUnexpectedType $
                             UnexpectedTypeError DocListType (docType doc) path
    Nothing  -> return $ ListDoc [] [] path


-- Value Parsers > Dictionary > Double
--------------------------------------------------------------------------------

atDoubleParser :: Text -> DictDoc -> ValueParser Double
atDoubleParser key (DictDoc hm _ path) = 
  case HM.lookup key hm of
    Just doc -> 
      case doc of
        DocNumber numDoc -> return $ toRealFloat $ numberDocValue numDoc
        _                -> Left $ ValueParseErrorUnexpectedType $
                              UnexpectedTypeError DocListType (docType doc) path
    Nothing  -> Left $ ValueParseErrorMissingKey $ MissingKeyError key path


-- Value Parsers > Dictionary > Text
--------------------------------------------------------------------------------

atTextParser :: Text -> DictDoc -> ValueParser Text
atTextParser key (DictDoc hm _ path) = 
  case HM.lookup key hm of
    Just doc -> 
      case doc of
        DocText text -> return $ textDocValue text
        _            -> Left $ ValueParseErrorUnexpectedType $
                          UnexpectedTypeError DocTextType (docType doc) path
    Nothing  -> Left $ ValueParseErrorMissingKey $ MissingKeyError key path






-- atTextListParser :: Text -> DictDoc -> ValueParser [Text]
-- atTextListParser key (DictDoc hm _ path) = 
--   case HM.lookup key hm of
--     Just doc -> 
--       case doc of
--         DocList listDoc -> return listDoc
--         _               -> Left $ ValueParseErrorUnexpectedType $
--                              UnexpectedTypeError DocListType (docType doc) path
--     Nothing  -> Left $ ValueParseErrorMissingKey $ MissingKeyError key path




class FromDocument a where
  fromDocument :: Doc -> ValueParser a



instance FromDocument Text where
  fromDocument (DocText doc) = return $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


fromMaybeDocument :: FromDocument a => Maybe Doc -> ValueParser (Maybe a)
fromMaybeDocument Nothing    = return Nothing
fromMaybeDocument (Just doc) = Just <$> fromDocument doc




--------------------------------------------------------------------------------
-- PARSER
--------------------------------------------------------------------------------


type DocParser a = Either DocParseError a


data DocParseError = 
    UnexpectedYamlType UnexpectedYamlTypeError
  | UnknownPrimitiveType DocPath
  | TypeDoesNotExist Text DocPath
  | FieldMissing Text DocPath
  | SumTypeMissingType Text DocPath
  | SumTypeDoesNotHaveCase Text DocPath
  | SumTypeMissingValue Text Text DocPath


instance Show DocParseError where
  show (UnexpectedYamlType e) = show e
  show (UnknownPrimitiveType path) = 
      "Unknown Primitive Type:\n    " <> show path
  show (TypeDoesNotExist typeName path) = 
    "Type Does Not Exist:\n" <> 
    "    Type: " <> show typeName <> "\n" <> 
    "    Path: " <> show path
  show (FieldMissing fieldName path) = 
    "Field Missing:\n" <> 
    "   Field Name: " <> show fieldName <> "\n" <> 
    "   Path: " <> show path
  show (SumTypeMissingType typeName path) = 
    "Sum Type Missing Type:\n" <> 
    "   Type Name: " <> show typeName <> "\n" <> 
    "   Path: " <> show path
  show (SumTypeDoesNotHaveCase caseName path) = 
    "Sum Type Missing Type:\n" <> 
    "   Case: " <> show caseName <> "\n" <> 
    "   Path: " <> show path
  show (SumTypeMissingValue sumTypeName valueTypeName path) = 
    "Sum Type Missing Type:\n" <> 
    "   Sum Type: " <> show sumTypeName <> "\n" <> 
    "   Value Type: " <> show valueTypeName <> "\n" <> 
    "   Path: " <> show path


data UnexpectedYamlTypeError = UnexpectedYamlTypeError
  { expectedYamlType :: YamlType
  , actualYamlType :: YamlType
  , unexpectedYamlTypePath :: DocPath
  }


instance Show UnexpectedYamlTypeError where
  show (UnexpectedYamlTypeError expected actual path) = 
    "Unexpected Yaml Type:\n" <>
    "    Expected: " <> show expected <> "\n" <>
    "    Actual: " <> show actual <> "\n" <>
    "    Path: " <> show path


data YamlType = 
    YamlObject 
  | YamlArray 
  | YamlString 
  | YamlNumber 
  | YamlBool 
  | YamlNull


instance Show YamlType where
  show YamlObject = "Object"
  show YamlArray  = "Array"
  show YamlString = "String"
  show YamlNumber = "Number"
  show YamlBool   = "Bool"
  show YamlNull   = "Null"


yamlType :: Yaml.Value -> YamlType
yamlType (Yaml.Object _) = YamlObject
yamlType (Yaml.Array  _) = YamlArray
yamlType (Yaml.String _) = YamlString
yamlType (Yaml.Number _) = YamlNumber
yamlType (Yaml.Bool   _) = YamlBool
yamlType Yaml.Null       = YamlNull

