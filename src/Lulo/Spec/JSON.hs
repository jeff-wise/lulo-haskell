
{-| FromJSON instances for Lulo Types

    Orphaned JSON instances for Lulo Types. They are defined separately to 
    promote modularity. This allows any uers of the library to provide their 
    own instances without newtypes by not importing this module. Of course, 
    they will have to be wary of Overlapping Instances, but that should be 
    easily avoided.
-}


{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Lulo.Spec.JSON where


import Lulo.Spec.Types

import qualified Data.Aeson.Types as YAML
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml ((.:), (.:?), Parser, FromJSON, parseJSON)



--------------------------------------------------------------------------------
-- SPECIFICATION
--------------------------------------------------------------------------------

instance FromJSON Spec where
  parseJSON (YAML.Object hm) = Spec
                           <$> hm .: "version"
                           <*> hm .: "metadata"
                           <*> hm .: "description"
                           <*> hm .: "root_type"
                           <*> hm .: "types"
                           <*> (concat . maybeToList <$> hm .:? "constraints")
  parseJSON invalid    = YAML.typeMismatch "Spec" invalid


-- Specification > Version
--------------------------------------------------------------------------------

instance FromJSON SpecVersion where
  parseJSON (YAML.String s) = return $ SpecVersion s
  parseJSON invalid         = YAML.typeMismatch "SpecVersion" invalid


-- Specification > Metadata
--------------------------------------------------------------------------------

instance FromJSON SpecMetadata where
  parseJSON (YAML.Object hm) = SpecMetadata
                           <$> hm .: "name"
                           <*> (concat . maybeToList <$> hm .:? "authors")
  parseJSON invalid          = YAML.typeMismatch "SpecMetadata" invalid


-- Specification > Metadata > Name
--------------------------------------------------------------------------------

instance FromJSON SpecName where
  parseJSON (YAML.String s) = return $ SpecName s
  parseJSON invalid         = YAML.typeMismatch "SpecName" invalid


-- Specification > Metadata > Author
--------------------------------------------------------------------------------

instance FromJSON SpecAuthor where
  parseJSON (YAML.Object hm) = SpecAuthor <$> hm .: "name"
  parseJSON invalid          = YAML.typeMismatch "SpecAuthor" invalid


-- Specification > Description
--------------------------------------------------------------------------------

instance FromJSON SpecDescription where
  parseJSON (YAML.Object hm) = SpecDescription <$> hm .: "overview_md"
  parseJSON invalid          = YAML.typeMismatch "SpecDescription" invalid


--------------------------------------------------------------------------------
-- CUSTOM TYPE
--------------------------------------------------------------------------------

instance FromJSON CustomType where
  parseJSON obj@(YAML.Object m) = do
    -- Custom Type Data
    customTypeData <- parseJSON obj :: Parser CustomTypeData
    -- Type
    kind <- m .: "type" :: Parser Text
    _customType <- case kind of
                     "product"   -> CustomTypeProduct <$> parseJSON obj
                     "sum"       -> CustomTypeSum <$> parseJSON obj
                     "primitive" -> CustomTypePrim <$> parseJSON obj
                     _           -> fail $ "Object type must be either " ++
                                          "'product', 'sum', or 'primitive'"
    return $ CustomType customTypeData _customType
  parseJSON invalid             = YAML.typeMismatch "CustomType" invalid 


-- Custom Type > Data
--------------------------------------------------------------------------------

instance FromJSON CustomTypeData where
  parseJSON (YAML.Object m) = CustomTypeData
                          <$> m .:  "name" 
                          <*> m .:  "label" 
                          <*> m .:? "description" 
                          <*> m .:? "group" 
                          <*> (concat . maybeToList <$> m .:? "examples_yaml")
  parseJSON invalid    = YAML.typeMismatch "CustomTypeData" invalid


-- Custom Type > Data > Name
--------------------------------------------------------------------------------

instance FromJSON CustomTypeName where
  parseJSON (YAML.String s) = return $ CustomTypeName s
  parseJSON invalid         = YAML.typeMismatch "CustomTypeName" invalid


-- Custom Type > Data > Label
--------------------------------------------------------------------------------

instance FromJSON CustomTypeLabel where
  parseJSON (YAML.String s) = return $ CustomTypeLabel s
  parseJSON invalid         = YAML.typeMismatch "CustomTypeLabel" invalid


-- Custom Type > Data > Description
--------------------------------------------------------------------------------

instance FromJSON CustomTypeDescription where
  parseJSON (YAML.String s) = return $ CustomTypeDescription s
  parseJSON invalid         = YAML.typeMismatch "CustomTypeDescription" invalid


-- Custom Type > Data > Group
--------------------------------------------------------------------------------

instance FromJSON CustomTypeGroup where
  parseJSON (YAML.String s) = return $ CustomTypeGroup s
  parseJSON invalid         = YAML.typeMismatch "CustomTypeGroup" invalid


--------------------------------------------------------------------------------
-- CUSTOM TYPE > PRODUCT
--------------------------------------------------------------------------------

instance FromJSON ProductCustomType where
  parseJSON (YAML.Object hm) = ProductCustomType <$> hm .: "fields"
  parseJSON invalid          = YAML.typeMismatch "ProductCustomType" invalid


-- Custom Type > Product > Field
--------------------------------------------------------------------------------

instance FromJSON Field where
  parseJSON obj@(YAML.Object m) = Field 
                              <$> m .: "name"
                              <*> m .: "presence"
                              <*> m .:? "description"
                              <*> parseJSON obj
                              <*> (concat . maybeToList <$> m .:? "constraints")
                              <*> m .:? "default_value"
  parseJSON invalid             = YAML.typeMismatch "Field" invalid


-- Custom Type > Product > Field > Name
--------------------------------------------------------------------------------

instance FromJSON FieldName where
  parseJSON (YAML.String s) = return $ FieldName s
  parseJSON invalid         = YAML.typeMismatch "FieldName" invalid


-- Custom Type > Product > Field > Description
--------------------------------------------------------------------------------

instance FromJSON FieldDescription where
  parseJSON (YAML.String s) = return $ FieldDescription s
  parseJSON invalid         = YAML.typeMismatch "FieldDescription" invalid


-- Custom Type > Product > Field > Default Value
--------------------------------------------------------------------------------

instance FromJSON FieldDefaultValue where
  parseJSON (YAML.String s) = return $ FieldDefaultValue s
  parseJSON invalid         = YAML.typeMismatch "FieldDefaultValue" invalid


-- Custom Type > Product > Field > Presence
--------------------------------------------------------------------------------

instance FromJSON FieldPresence where
  parseJSON (YAML.String s) = do
    let failMessage = "Presence must be either 'optional' or 'required'"
    case s of
      "optional" -> return Optional
      "required" -> return Required
      _          -> fail failMessage
  parseJSON invalid     = YAML.typeMismatch "Presence" invalid


--------------------------------------------------------------------------------
-- CUSTOM TYPE > SUM
--------------------------------------------------------------------------------

instance FromJSON SumCustomType where
  parseJSON (YAML.Object hm) = SumCustomType <$> hm .: "cases"
  parseJSON invalid          = YAML.typeMismatch "SumCustomType" invalid


-- Custom Type > Sum > Case
--------------------------------------------------------------------------------

instance FromJSON Case where
  parseJSON (YAML.Object hm) = Case 
                           <$> hm .: "type" 
                           <*> hm .:? "description" 
  parseJSON invalid          = YAML.typeMismatch "Case" invalid

-- Custom Type > Sum > Case > Description
--------------------------------------------------------------------------------

instance FromJSON CaseDescription where
  parseJSON (YAML.String s) = return $ CaseDescription s
  parseJSON invalid         = YAML.typeMismatch "CaseDescription" invalid


--------------------------------------------------------------------------------
-- CUSTOM TYPE > PRIMITIVE
--------------------------------------------------------------------------------

instance FromJSON PrimCustomType where
  parseJSON (YAML.Object hm) = PrimCustomType 
                           <$> hm .: "base_type" 
                           <*> (concat . maybeToList <$> hm .:? "constraints")
  parseJSON invalid          = YAML.typeMismatch "PrimCustomType" invalid


--------------------------------------------------------------------------------
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
                   customTypeName -> return $ Custom $ CustomTypeName customTypeName
  parseJSON invalid         = YAML.typeMismatch "ValueType" invalid


-- Value Type > Primitive
--------------------------------------------------------------------------------

instance FromJSON PrimValueType where
  parseJSON (YAML.String s) = 
    case s of
      "any"     -> return Any
      "number"  -> return Number
      "string"  -> return String
      "boolean" -> return Boolean
      other     -> fail $ "Unknown Primitive Value Type: " ++ T.unpack other
  parseJSON invalid         = YAML.typeMismatch "PrimValueType" invalid


--------------------------------------------------------------------------------
-- CONSTRAINT
--------------------------------------------------------------------------------

instance FromJSON Constraint where
  parseJSON obj@(YAML.Object _) = Constraint 
                              <$> parseJSON obj
                              <*> parseJSON obj
  parseJSON invalid    = YAML.typeMismatch "Constraint" invalid 


-- Constraint > Data
--------------------------------------------------------------------------------

instance FromJSON ConstraintData where
  parseJSON (YAML.Object hm) = ConstraintData
                           <$> hm .:  "name" 
                           <*> hm .:? "description" 
  parseJSON invalid         = YAML.typeMismatch "ConstraintData" invalid 


-- Constraint > Data > Name
--------------------------------------------------------------------------------

instance FromJSON ConstraintName where
  parseJSON (YAML.String s) = return $ ConstraintName s
  parseJSON invalid         = YAML.typeMismatch "ConstraintName" invalid


-- Constraint > Data > Description
--------------------------------------------------------------------------------

instance FromJSON ConstraintDescription where
  parseJSON (YAML.String s) = return $ ConstraintDescription s
  parseJSON invalid         = YAML.typeMismatch "ConstraintDescription" invalid


-- Constraint > Constraint'
--------------------------------------------------------------------------------

instance FromJSON Constraint' where
  parseJSON (YAML.Object m) = do 
    typeString <- m .: "type" :: Parser Text
    paramtersValue <- m .: "parameters" :: Parser YAML.Value
    let failMessage = "Invalid constraint name. Please check the" ++
                      " documentation for a list of valid constraints."
    case typeString of
      "string_one_of"       -> StringOneOf <$> parseJSON paramtersValue
      "number_greater_than" -> NumGreaterThan <$> parseJSON paramtersValue
      _                     -> fail failMessage
  parseJSON invalid         = YAML.typeMismatch "Constraint'" invalid 

 
-- Constraint > String One Of
--------------------------------------------------------------------------------

instance FromJSON StringOneOfConstraint where
  parseJSON (YAML.Object m) = StringOneOfConstraint
                              <$> m .: "set" 
  parseJSON invalid         = YAML.typeMismatch "StringOneOfConstraint" invalid 


-- Constraint > Number Greater Than
--------------------------------------------------------------------------------

instance FromJSON NumberGreaterThanConstraint where
  parseJSON (YAML.Object m) = NumberGreaterThanConstraint
                              <$> m .: "greater_than" 
  parseJSON invalid         = YAML.typeMismatch "NumberGreaterThanConstraint" invalid 

