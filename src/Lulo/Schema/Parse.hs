
{-| Schema Parsing

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Schema.Parse where


import Lulo.Doc.Parser
import Lulo.Doc.Types
import Lulo.Schema.Index
import Lulo.Schema.Types


import Control.Monad (unless)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM 
  ( fromList
  , lookup
  )
import Data.Maybe (catMaybes, listToMaybe)
-- import Data.Monoid
import Data.Text (Text)
-- import qualified Data.Text as T (unpack)
import Data.Traversable (forM)
import qualified Data.Vector as Vec (toList, imapM)
import qualified Data.Yaml as Yaml (Value (..))

-- import Debug.Trace (trace)



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
    Yaml.Array valueVec -> do
      let mType = lookupSchemaType customTypeName (schema:schemaDeps)
      case mType of
        Just _type -> DocList . newListDoc . Vec.toList <$> 
                        Vec.imapM (itemParser _type) valueVec 
        Nothing    -> Left $ TypeDoesNotExist (getCustomTypeName customTypeName) path
    Yaml.Null -> return $ DocList $ ListDoc [] cases path
    _         ->
      let err = UnexpectedYamlTypeError YamlObject (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err
  where
    itemParser :: CustomType -> Int -> Yaml.Value -> DocParser Doc
    itemParser _type i itemYamlValue = do
      let newPath = pathWithIndex i path
      valueParser _type itemYamlValue cases newPath schema schemaDeps
    newListDoc :: [Doc] -> ListDoc
    newListDoc hm = ListDoc hm cases path


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
productParser productType yamlValue cases path schema schemaDeps = 
  case yamlValue of
    Yaml.Object hm -> fieldsParser hm
    _              ->
      let err = UnexpectedYamlTypeError YamlObject (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err
  where
    newDictDoc :: HashMap Text Doc -> DictDoc
    newDictDoc hm = DictDoc hm cases path
    fieldsParser :: HashMap Text Yaml.Value -> DocParser Doc
    fieldsParser hm = DocDict . newDictDoc . HM.fromList . catMaybes <$> 
                        forM (prodTypeFields productType) (`parseField` hm)
    parseField :: Field -> HashMap Text Yaml.Value -> DocParser (Maybe (Text, Doc))
    parseField field hm =
      let keyText = getFieldName $ fieldName field 
          maybeFieldYamlValue = HM.lookup keyText hm 
      in  case maybeFieldYamlValue of
            Just fieldYamlValue -> do 
              let newPath = pathWithKey keyText path
              doc <- fieldParser field fieldYamlValue newPath schema schemaDeps
              return $ Just (keyText, doc)
            Nothing             -> 
              case fieldPresence field of
                Required -> Left $ FieldMissing (getFieldName $ fieldName field) path
                Optional -> return Nothing


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
    Prim       primitiveType  -> primitiveParser primitiveType yamlValue [] path
    PrimList   primitiveType  -> primitiveListParser primitiveType yamlValue [] path
    Custom     customTypeName -> 
      let mType = lookupSchemaType customTypeName (schema:schemaDeps)
      in  case mType of
            Just _type -> valueParser _type yamlValue [] path schema schemaDeps
            Nothing    -> Left $ TypeDoesNotExist (getCustomTypeName customTypeName) path
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
             (Left $ SumTypeDoesNotHaveCase (getCustomTypeName caseTypeName) path)
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
    Nothing            -> Left $ SumTypeMissingType (getCustomTypeName _typeName) path


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
      Left $ SumTypeMissingValue (getCustomTypeName valueTypeName) 
                                 (getCustomTypeName _sumTypeName) 
                                 path 


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
synonymParser synType yamlValue cases path schema schemaDeps = 
  case primTypeBaseType synType of
    BaseTypePrim   primValueType  -> primitiveParser primValueType yamlValue cases path
    BaseTypeCustom customTypeName ->
      let mType = lookupSchemaType customTypeName (schema:schemaDeps)
      in  case mType of
            Just _type -> valueParser _type yamlValue cases path schema schemaDeps
            Nothing    -> Left $ TypeDoesNotExist (getCustomTypeName customTypeName) path


--------------------------------------------------------------------------------
-- PRIMITIVE PARSERS
--------------------------------------------------------------------------------

primitiveParser :: PrimValueType -> 
                   Yaml.Value -> 
                   [DocCase] -> 
                   DocPath -> 
                   DocParser Doc
primitiveParser primitiveType yamlValue cases path = 
  case primitiveType of
    Number  -> numberParser yamlValue cases path
    String  -> stringParser yamlValue cases path
    Boolean -> booleanParser yamlValue cases path
    _       -> Left $ UnknownPrimitiveType path


primitiveListParser :: PrimValueType -> 
                       Yaml.Value -> 
                       [DocCase] -> 
                       DocPath -> 
                       DocParser Doc
primitiveListParser primitiveType yamlValue cases path =
  case yamlValue of
    Yaml.Array valueVec -> DocList . newListDoc . Vec.toList <$> 
                            forM valueVec (\itemYamlValue ->
                              primitiveParser primitiveType itemYamlValue cases path)
    _                   ->  
      let err = UnexpectedYamlTypeError YamlArray (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err
  where 
    newListDoc :: [Doc] -> ListDoc
    newListDoc hm = ListDoc hm [] path


-- Primitive > Number
--------------------------------------------------------------------------------

numberParser :: Yaml.Value -> [DocCase] -> DocPath -> DocParser Doc
numberParser yamlValue cases path =
  case yamlValue of
    Yaml.Number num -> Right $ DocNumber $ NumberDoc num cases path
    _               -> 
      let err = UnexpectedYamlTypeError YamlNumber (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err 


-- Primitive > String
--------------------------------------------------------------------------------

stringParser :: Yaml.Value -> [DocCase] -> DocPath -> DocParser Doc
stringParser yamlValue cases path =
  case yamlValue of
    Yaml.String text -> Right $ DocText $ TextDoc text cases path
    _               -> 
      let err = UnexpectedYamlTypeError YamlString (yamlType yamlValue) path
      in  Left $ UnexpectedYamlType err


-- Primitive > Boolean
--------------------------------------------------------------------------------

booleanParser :: Yaml.Value -> [DocCase] -> DocPath -> DocParser Doc
booleanParser yamlValue cases path  =
  case yamlValue of
    Yaml.Bool bool -> Right $ DocBoolean $ BooleanDoc bool cases path
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
    Nothing         -> Left $ TypeDoesNotExist (getCustomTypeName _typeName ) path
