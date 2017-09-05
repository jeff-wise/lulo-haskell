
{-| Schema Parsing

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Schema.Parse where


import Lulo.Document
import Lulo.Schema.Index
import Lulo.Schema.Types


import Control.Monad (unless)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM 
  ( fromList
  , lookup
  )
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Traversable (forM)
import qualified Data.Vector as Vec (toList)
import qualified Data.Yaml as Yaml (Value (..))



--------------------------------------------------------------------------------
-- TYPE PARSER
--------------------------------------------------------------------------------

valueParser :: CustomType -> 
               Yaml.Value -> 
               [DocCase] -> 
               DocPath -> 
               SchemaIndex -> 
               [SchemaIndex] -> 
               DocParser Doc
valueParser customType yamlValue cases path schema schemaDeps = 
  case customType of
    CustomTypeProduct productType -> 
        productParser productType yamlValue cases path schema schemaDeps
    CustomTypeSum     sumType     -> 
        sumParser sumType yamlValue cases path schema schemaDeps
    CustomTypePrim    wrapType -> 
        synonymParser wrapType yamlValue cases path schema schemaDeps


valueListParser :: CustomTypeName -> 
                   Yaml.Value -> 
                   [DocCase] -> 
                   DocPath -> 
                   SchemaIndex -> 
                   [SchemaIndex] -> 
                   DocParser Doc
valueListParser customTypeName yamlValue cases path schema schemaDeps =
  case yamlValue of
    Yaml.Array valueVec -> 
      let mType = lookupSchemaType customTypeName (schema:schemaDeps)
      in  case mType of
            Just _type -> DocList . Vec.toList <$>
                            forM valueVec (\itemYamlValue ->
                              valueParser _type itemYamlValue cases path schema schemaDeps)
            Nothing    -> Left $ TypeDoesNotExist customTypeName path
    _          ->
      let err = UnexpectedYamlTypeError YamlObject (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err


--------------------------------------------------------------------------------
-- PRODUCT TYPE PARSER
--------------------------------------------------------------------------------

productParser :: ProductCustomType -> 
                 Yaml.Value -> 
                 [DocCase] -> 
                 DocPath -> 
                 SchemaIndex -> 
                 [SchemaIndex] -> 
                 DocParser Doc
productParser productType yamlValue _ path schema schemaDeps = 
  case yamlValue of
    Yaml.Object hm -> fieldsParser hm
    _              ->
      let err = UnexpectedYamlTypeError YamlObject (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err
  where
    fieldsParser :: HashMap Text Yaml.Value -> DocParser Doc
    fieldsParser hm = DocDict . HM.fromList <$> 
                        forM (prodTypeFields productType) (`parseField` hm)
    parseField :: Field -> HashMap Text Yaml.Value -> DocParser (Text, Doc)
    parseField field hm =
      let keyText = getFieldName$ fieldName field 
          maybeFieldYamlValue = HM.lookup keyText hm 
      in  case maybeFieldYamlValue of
            Just fieldYamlValue -> do 
              doc <- fieldParser field fieldYamlValue path schema schemaDeps
              return (keyText, doc)
            Nothing             -> Left $ FieldMissing (fieldName field) path


-- Product > Field Parser
--------------------------------------------------------------------------------

fieldParser :: Field -> 
               Yaml.Value -> 
               DocPath -> 
               SchemaIndex -> 
               [SchemaIndex] -> 
               DocParser Doc
fieldParser field yamlValue path schema schemaDeps = 
  case fieldValueType field of
    Prim       primitiveType  -> primitiveParser primitiveType yamlValue path
    PrimList   primitiveType  -> primitiveListParser primitiveType yamlValue path
    Custom     customTypeName -> 
      let mType = lookupSchemaType customTypeName (schema:schemaDeps)
      in  case mType of
            Just _type -> valueParser _type yamlValue [] path schema schemaDeps
            Nothing    -> Left $ TypeDoesNotExist customTypeName path
    CustomList customTypeName -> valueListParser customTypeName 
                                                 yamlValue 
                                                 []
                                                 path 
                                                 schema 
                                                 schemaDeps


--------------------------------------------------------------------------------
-- SUM PARSER
--------------------------------------------------------------------------------

sumParser :: SumCustomType -> 
             Yaml.Value -> 
             [DocCase] -> 
             DocPath -> 
             SchemaIndex -> 
             [SchemaIndex] -> 
             DocParser Doc
sumParser sumType yamlValue cases path schema schemaDeps = 
  case yamlValue of
    Yaml.Object hm -> do
      typeNameYamlValue <- caseTypeNameYamlParser hm (sumTypeName sumType) path
      caseTypeName <- caseTypeNameParser typeNameYamlValue path
      unless (sumTypeHasCase caseTypeName sumType)
             (Left $ SumTypeDoesNotHaveCase caseTypeName path)
      _caseType <- schemaTypeParser caseTypeName (schema:schemaDeps) path
      caseValueYamlValue <- caseValueYamlValueParser hm 
                                                     caseTypeName 
                                                     (sumTypeName sumType) 
                                                     path
      let newCases = cases ++ [DocCase $ getCustomTypeName caseTypeName]
      valueParser _caseType caseValueYamlValue newCases path schema schemaDeps
    _              -> 
      let err = UnexpectedYamlTypeError YamlObject (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err 


caseTypeNameYamlParser :: HashMap Text Yaml.Value -> 
                          CustomTypeName -> 
                          DocPath -> 
                          DocParser Yaml.Value
caseTypeNameYamlParser hm _typeName path = 
  case HM.lookup "type" hm of
    Just typeYamlValue -> return typeYamlValue
    Nothing            -> Left $ SumTypeMissingType _typeName path


caseTypeNameParser :: Yaml.Value -> DocPath -> DocParser CustomTypeName
caseTypeNameParser yamlValue path = 
  case yamlValue of
    Yaml.String text -> return $ CustomTypeName text
    _                ->
      let err = UnexpectedYamlTypeError YamlObject (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err 


caseValueYamlValueParser :: HashMap Text Yaml.Value -> 
                            CustomTypeName -> 
                            CustomTypeName -> 
                            DocPath ->
                            DocParser Yaml.Value
caseValueYamlValueParser hm valueTypeName _sumTypeName path = do
  let valueTypeNameText = getCustomTypeName valueTypeName
  case HM.lookup valueTypeNameText hm of
    Just valueYamlValue -> return valueYamlValue
    Nothing             -> 
      Left $ SumTypeMissingValue valueTypeName _sumTypeName path 


--------------------------------------------------------------------------------
-- SYNONYM PARSER
--------------------------------------------------------------------------------

synonymParser :: PrimCustomType -> 
                 Yaml.Value -> 
                 [DocCase] -> 
                 DocPath -> 
                 SchemaIndex -> 
                 [SchemaIndex] -> 
                 DocParser Doc
synonymParser synType yamlValue _ path _ _ = 
  primitiveParser (primTypeBaseType synType) yamlValue path


--------------------------------------------------------------------------------
-- PRIMITIVE PARSERS
--------------------------------------------------------------------------------

primitiveParser :: PrimValueType -> 
                   Yaml.Value -> 
                   DocPath -> 
                   DocParser Doc
primitiveParser primitiveType yamlValue path = 
  case primitiveType of
    Number  -> numberParser yamlValue path
    String  -> stringParser yamlValue path
    Boolean -> booleanParser yamlValue path
    _       -> Left $ UnknownPrimitiveType path


primitiveListParser :: PrimValueType -> Yaml.Value -> DocPath -> DocParser Doc
primitiveListParser primitiveType yamlValue path =
  case yamlValue of
    Yaml.Array valueVec -> DocList . Vec.toList <$> 
                            forM valueVec (\itemYamlValue ->
                              primitiveParser primitiveType itemYamlValue path)
    _                   ->  
      let err = UnexpectedYamlTypeError YamlArray (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err



-- Primitive > Number
--------------------------------------------------------------------------------

numberParser :: Yaml.Value -> DocPath -> DocParser Doc
numberParser yamlValue path =
  case yamlValue of
    Yaml.Number num -> Right $ DocNumber num
    _               -> 
      let err = UnexpectedYamlTypeError YamlNumber (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err 


-- Primitive > String
--------------------------------------------------------------------------------

stringParser :: Yaml.Value -> DocPath -> DocParser Doc
stringParser yamlValue path =
  case yamlValue of
    Yaml.String text -> Right $ DocText text
    _               -> 
      let err = UnexpectedYamlTypeError YamlString (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err


-- Primitive > Boolean
--------------------------------------------------------------------------------

booleanParser :: Yaml.Value -> DocPath -> DocParser Doc
booleanParser yamlValue path  =
  case yamlValue of
    Yaml.Bool bool -> Right $ DocBoolean bool
    _               -> 
      let err = UnexpectedYamlTypeError YamlBool (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err



lookupSchemaType :: CustomTypeName -> [SchemaIndex] -> Maybe CustomType
lookupSchemaType _typeName schemaIndexes = 
  let types = catMaybes $ fmap (`typeWithName` _typeName) schemaIndexes
  in  listToMaybe types


schemaTypeParser :: CustomTypeName -> 
                    [SchemaIndex] -> 
                    DocPath -> 
                    DocParser CustomType
schemaTypeParser _typeName schemaIndexes path =
  case lookupSchemaType _typeName schemaIndexes of
    Just schemaType -> return schemaType
    Nothing         -> Left $ TypeDoesNotExist _typeName path
