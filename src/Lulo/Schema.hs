
{-| Types
-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Schema where


import Lulo.Doc.Parser (DocParseError, fromDocument)
import Lulo.Doc.Types (Doc, emptyDocPath)
import Lulo.Schema.Index
import Lulo.Schema.Parse (lookupSchemaType, valueParser)
import Lulo.Schema.SchemaSchema
import Lulo.Schema.Types
import Lulo.Value (ValueParseError)

import Data.Maybe (fromJust)
import Data.Monoid
import qualified Data.Yaml as Yaml
  ( Value
  , ParseException
  , decodeFileEither
  )



--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

-- | Try to parse a schema file
parseSchemaFile :: FilePath -> IO (Either SchemaParseError Schema)
parseSchemaFile schemaFilePath = do
  eYamlValue <- Yaml.decodeFileEither schemaFilePath 
                  :: IO (Either Yaml.ParseException Yaml.Value)
  case eYamlValue of
    Right yamlValue -> return $ parseSchemaDocument yamlValue
    Left  ex        -> return $ Left $ SchemaYamlError ex


-- | Try to parse a schema yaml value
parseSchemaDocument :: Yaml.Value -> Either SchemaParseError Schema
parseSchemaDocument yamlValue = do
  let schemaSchemaIndex = schemaIndex schemaSchema
      schemaType = fromJust $ lookupSchemaType (CustomTypeName "schema") 
                                               [schemaSchemaIndex]
  schemaDoc <- schemaDocParser schemaSchemaIndex schemaType
  case fromDocument schemaDoc of
    Right schema -> return schema 
    Left  err    -> Left $ SchemaConversionError err
  where
    schemaDocParser :: SchemaIndex -> CustomType -> Either SchemaParseError Doc
    schemaDocParser schemaSchemaIndex schemaType = do
      let docParser = valueParser schemaType 
                                  yamlValue 
                                  [] 
                                  emptyDocPath 
                                  schemaSchemaIndex 
                                  []
      case docParser of
        Right doc -> return doc
        Left  err -> Left $ SchemaFormatError err  


--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

data SchemaParseError = 
    SchemaYamlError Yaml.ParseException
  | SchemaFormatError DocParseError
  | SchemaConversionError ValueParseError


instance Show SchemaParseError where
  show (SchemaYamlError yamlException) = 
      "Yaml Error: " <> show yamlException
  show (SchemaFormatError docParseError) = show docParseError 
  show (SchemaConversionError valueParseError) = show valueParseError 
    

