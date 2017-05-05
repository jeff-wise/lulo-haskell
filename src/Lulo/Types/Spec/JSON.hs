
{-| FromJSON instances for Lulo Types

    Orphaned JSON instances for Lulo Types. They are defined separately to 
    promote modularity. This allows any uers of the library to provide their 
    own instances without newtypes by not importing this module. Of course, 
    they will have to be wary of Overlapping Instances, but that should be 
    easily avoided.
-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Types.Spec.JSON where


import Lulo.Types

import qualified Data.Aeson.Types as YAML
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Yaml ((.:), (.:?), Parser, FromJSON, parseJSON)



-- SPECIFICATION
--------------------------------------------------------------------------------

instance FromJSON Spec where
  parseJSON (YAML.Object o) = do
    version <- o .: "version"
    typeList <- o .: "types"
    constList <- o .: "constraints" :: Parser [ValueConstraint] 
    let constIndex = newConstraintIndex constList
    return $ Spec version typeList constIndex 
  parseJSON invalid    = YAML.typeMismatch "Spec" invalid


-- Specification > Version
--------------------------------------------------------------------------------

instance FromJSON SpecVersion where
  parseJSON n@(YAML.Number _) = SpecVersion <$> parseJSON n
  parseJSON invalid           = YAML.typeMismatch "SpecVersion" invalid


-- OBJECT TYPE
--------------------------------------------------------------------------------

instance FromJSON ObjectType where
  parseJSON obj@(YAML.Object m) = do
    -- Common Data
    commonData <- parseJSON obj :: Parser CommonTypeData
    -- Type specific data
    typeString <- m .: "type" :: Parser Text
    typeData <- case typeString of
                  "product" -> Product <$> parseJSON obj
                  "sum"     -> Sum <$> parseJSON obj
    return $ ObjectType commonData typeData
  parseJSON invalid    = YAML.typeMismatch "ObjectType" invalid 


-- Object Type > Common Type Data
--------------------------------------------------------------------------------

instance FromJSON CommonTypeData where
  parseJSON (YAML.Object m) = CommonTypeData
                          <$> m .:  "name" 
                          <*> m .:  "label" 
                          <*> m .:? "description" 
                          <*> m .:? "group" 
                          <*> (concat . maybeToList <$> m .:? "examples_yaml")
  parseJSON invalid    = YAML.typeMismatch "CommonTypeData" invalid


-- PRODUCT TYPE
--------------------------------------------------------------------------------

instance FromJSON ProductType where
  parseJSON obj@(YAML.Object hm) = ProductType 
                               <$> hm .: "fields"
  parseJSON invalid         = YAML.typeMismatch "ProductType" invalid


-- ProductType > Field
--------------------------------------------------------------------------------

instance FromJSON Field where
  parseJSON obj@(YAML.Object m) = Field 
                              <$> m .: "name"
                              <*> m .: "presence"
                              <*> m .:? "description"
                              <*> parseJSON obj
                              <*> (concat . maybeToList <$> m .:? "constraints")
                              <*> m .:? "default_value"
  parseJSON invalid         = YAML.typeMismatch "Field" invalid


-- ProductType > Field > Name
--------------------------------------------------------------------------------

instance FromJSON FieldName where
  parseJSON (YAML.String s) = return $ FieldName s
  parseJSON invalid         = YAML.typeMismatch "FieldName" invalid


-- ProductType > Field > Description
--------------------------------------------------------------------------------

instance FromJSON FieldDescription where
  parseJSON (YAML.String s) = return $ FieldDescription s
  parseJSON invalid         = YAML.typeMismatch "FieldDescription" invalid


-- ProductType > Field > Default Value
--------------------------------------------------------------------------------

instance FromJSON FieldDefaultValue where
  parseJSON (YAML.String s) = return $ FieldDefaultValue s
  parseJSON invalid         = YAML.typeMismatch "FieldDefaultValue" invalid


-- ProductType > Field > Presence
--------------------------------------------------------------------------------

instance FromJSON Presence where
  parseJSON (YAML.String s) = case s of
                                "optional" -> return Optional
                                "required" -> return Required
  parseJSON invalid     = YAML.typeMismatch "Presence" invalid


-- SUMTYPE
--------------------------------------------------------------------------------

instance FromJSON SumType where
  parseJSON obj@(YAML.Object hm) = SumType 
                               <$> hm .: "cases"
  parseJSON invalid              = YAML.typeMismatch "SumType" invalid


-- SumType > SumCase
--------------------------------------------------------------------------------

instance FromJSON SumCase where
  parseJSON obj@(YAML.Object hm) = SumCase 
                               <$> hm .: "name" 
                               <*> hm .:? "description" 
                               <*> parseJSON obj
  parseJSON invalid             = YAML.typeMismatch "SumCase" invalid


-- SumType > SumCase > Name
--------------------------------------------------------------------------------

instance FromJSON SumCaseName where
  parseJSON (YAML.String s) = return $ SumCaseName s
  parseJSON invalid         = YAML.typeMismatch "SumCaseName" invalid


-- SumType > SumCase > Description
--------------------------------------------------------------------------------

instance FromJSON SumCaseDescription where
  parseJSON (YAML.String s) = return $ SumCaseDescription s
  parseJSON invalid         = YAML.typeMismatch "SumCaseDescription" invalid


-- VALUE TYPE
--------------------------------------------------------------------------------

instance FromJSON ValueType where
  parseJSON (YAML.Object m) = do
    mOfText <- m .:? "of" :: Parser (Maybe Text)
    typeText <- m .: "type" :: Parser Text
    let mPrimType = textToPrimType typeText
    case mPrimType of
      Just primType -> return $ Prim primType
      Nothing -> case typeText of
                   "list"     -> case mOfText of
                                   Just ofText -> return $ listType ofText
                                   Nothing     -> fail "List type declared without 'of' "
                   customType -> return $ Custom $ CustomTypeName customType 
  parseJSON invalid    = YAML.typeMismatch "CustomTypeName" invalid



-- ValueType > Custom Type Name
--------------------------------------------------------------------------------

instance FromJSON CustomTypeName where
  parseJSON (YAML.String s) = return $ CustomTypeName s
  parseJSON invalid         = YAML.typeMismatch "CustomTypeName" invalid


-- VALUE CONSTRAINT
--------------------------------------------------------------------------------

instance FromJSON ValueConstraint where
  parseJSON obj@(YAML.Object m) = ValueConstraint 
                              <$> parseJSON obj
                              <*> parseJSON obj
  parseJSON invalid    = YAML.typeMismatch "ValueConstraint" invalid 


-- Value Constraint > Common Constraint Data
--------------------------------------------------------------------------------

instance FromJSON CommonConstraintData where
  parseJSON (YAML.Object m) = CommonConstraintData
                          <$> m .:  "name" 
                          <*> m .:? "description" 
  parseJSON invalid         = YAML.typeMismatch "CommonConstraintData" invalid 


-- Value Constraint > Common Constraint Data > Name
--------------------------------------------------------------------------------

instance FromJSON ConstraintName where
  parseJSON (YAML.String s) = return $ ConstraintName s
  parseJSON invalid         = YAML.typeMismatch "ConstraintName" invalid


-- Value Constraint > Common Constraint Data > Description
--------------------------------------------------------------------------------

instance FromJSON ConstraintDescription where
  parseJSON (YAML.String s) = return $ ConstraintDescription s
  parseJSON invalid         = YAML.typeMismatch "ConstraintDescription" invalid


-- Value Constraint > Constraint
--------------------------------------------------------------------------------

instance FromJSON Constraint where
  parseJSON (YAML.Object m) = do 
    typeString <- m .: "type" :: Parser Text
    paramtersValue <- m .: "parameters" :: Parser YAML.Value
    case typeString of
      "string_one_of"       -> StringOneOf <$> parseJSON paramtersValue
      "number_greater_than" -> NumGreaterThan <$> parseJSON paramtersValue
  parseJSON invalid         = YAML.typeMismatch "CommonConstraintData" invalid 

 
-- Value Constraint > Constraint > String One Of
--------------------------------------------------------------------------------

instance FromJSON StringOneOfConstraint where
  parseJSON obj@(YAML.Object m) = StringOneOfConstraint
                              <$> m .: "set" 
  parseJSON invalid         = YAML.typeMismatch "StringOneOfConstraint" invalid 


-- Value Constraint > Constraint > Number Greater Than
--------------------------------------------------------------------------------

instance FromJSON NumberGreaterThanConstraint where
  parseJSON obj@(YAML.Object m) = NumberGreaterThanConstraint
                              <$> m .: "greater_than" 
  parseJSON invalid         = YAML.typeMismatch "NumberGreaterThanConstraint" invalid 


