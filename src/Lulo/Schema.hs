
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



parseSchemaFile :: FilePath -> IO (Either DocumentParseError Schema)
parseSchemaFile schemaFilePath = do
  eYamlValue <- Yaml.decodeFileEither schemaFilePath 
                  :: IO (Either Yaml.ParseException Yaml.Value)
  case eYamlValue of
    Right yamlValue -> return $ parseSchemaDocument yamlValue
    Left  ex        -> return $ Left $ DocumentYamlError ex


parseSchemaDocument :: Yaml.Value -> Either DocumentParseError Schema
parseSchemaDocument yamlValue = do
  let schemaSchemaIndex = schemaIndex schemaSchema
      schemaType = fromJust $ lookupSchemaType (CustomTypeName "schema") 
                                               [schemaSchemaIndex]
  schemaDoc <- schemaDocParser schemaSchemaIndex schemaType
  case fromDocument schemaDoc of
    Right schema -> return schema 
    Left  err    -> Left $ DocumentConversionError err
  where
    schemaDocParser :: SchemaIndex -> CustomType -> Either DocumentParseError Doc
    schemaDocParser schemaSchemaIndex schemaType = do
      let docParser = valueParser schemaType 
                                  yamlValue 
                                  [] 
                                  emptyDocPath 
                                  schemaSchemaIndex 
                                  []
      case docParser of
        Right doc -> return doc
        Left  err -> Left $ DocumentFormatError err  



data DocumentParseError = 
    DocumentYamlError Yaml.ParseException
  | DocumentFormatError DocParseError
  | DocumentConversionError ValueParseError


instance Show DocumentParseError where
  show (DocumentYamlError yamlException) = 
      "Yaml Error: " <> show yamlException
  show (DocumentFormatError docParseError) = show docParseError 
  show (DocumentConversionError valueParseError) = show valueParseError 
    

