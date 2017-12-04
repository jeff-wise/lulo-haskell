
{-| Lulo Document

-}


module Lulo.Document where


import Lulo.Doc.Parser (DocParseError, FromDocument (..))
import Lulo.Doc.Types (Doc, emptyDocPath)
import Lulo.Schema.Index
import Lulo.Schema.Parse (lookupSchemaType, valueParser)
import Lulo.Schema.Types (Schema, schemaRootTypeName)
import Lulo.Value (ValueParseError)

import Data.Maybe (fromJust)
import Data.Monoid ((<>))

import qualified Data.Yaml as Yaml
  ( Value
  , ParseException
  , decodeFileEither
  )



--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

-- | Try to parse a document file using the given schema
parseDocumentFile :: FilePath -> Schema -> IO (Either DocumentParseError Doc)
parseDocumentFile documentFilePath schema = do
  eYamlValue <- Yaml.decodeFileEither documentFilePath 
                  :: IO (Either Yaml.ParseException Yaml.Value)
  case eYamlValue of
    Right yamlValue -> return $ parseDocument yamlValue schema
    Left  ex        -> return $ Left $ DocumentYamlError ex


-- | Try to parse a document using the given schema
parseDocument :: Yaml.Value -> Schema -> Either DocumentParseError Doc
parseDocument yamlValue schema = do
  let schemaSchemaIndex = schemaIndex schema
      schemaType = fromJust $ lookupSchemaType (schemaRootTypeName schema)
                                               [schemaSchemaIndex]
      docParser = valueParser schemaType 
                              yamlValue 
                              [] 
                              emptyDocPath 
                              schemaSchemaIndex 
                              []
  case docParser of
    Right doc -> return doc
    Left  err -> Left $ DocumentFormatError err  


parseDocumentFileValue :: FromDocument a => FilePath -> 
                                            Schema -> 
                                            IO (Either DocumentParseError a)
parseDocumentFileValue documentFilePath schema = do
  eYamlValue <- Yaml.decodeFileEither documentFilePath 
                  :: IO (Either Yaml.ParseException Yaml.Value)
  case eYamlValue of
    Right yamlValue -> return $ parseDocumentValue yamlValue schema
    Left  ex        -> return $ Left $ DocumentYamlError ex


parseDocumentValue :: FromDocument a => Yaml.Value -> Schema -> Either DocumentParseError a
parseDocumentValue yamlValue schema = do
  doc <- parseDocument yamlValue schema
  case fromDocument doc of
    Left  err -> Left $ DocumentConversionError err
    Right a   -> Right a 


--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

data DocumentParseError = 
    DocumentYamlError Yaml.ParseException
  | DocumentFormatError DocParseError
  | DocumentConversionError ValueParseError


instance Show DocumentParseError where
  show (DocumentYamlError yamlException) = 
      "Yaml Error: " <> show yamlException
  show (DocumentFormatError docParseError) = show docParseError 
  show (DocumentConversionError valueParseError) = show valueParseError 
    
