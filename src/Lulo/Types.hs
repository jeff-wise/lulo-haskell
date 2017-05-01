
{-| Types
-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Types where


import qualified Data.Aeson.Types as YAML
import Data.HashSet (HashSet)
import Data.Text
import Data.Yaml ((.:), (.:?), Parser, FromJSON, parseJSON)
import qualified Data.Yaml as YAML



-- APPLICATION
--------------------------------------------------------------------------------

-- > PARAMETERS
--------------------------------------------------------------------------------

data Parameters = Parameters 
  { filename  :: FilePath 
  , showDebug :: Bool
  }


-- SPECIFICATION
--------------------------------------------------------------------------------

-- ** Spec
--------------------------------------------------------------------------------

data Spec = Spec
  { version     :: SpecVersion
  , types       :: [Type]
  , constraints :: [Constraint]
  }


instance FromJSON Spec where
  parseJSON (YAML.Object o) = Spec
                          <$> o .: "version"
                          <*> o .: "types"
                          <*> o .: "constraints"
  parseJSON invalid    = YAML.typeMismatch "Spec" invalid


-- ** Spec Version
--------------------------------------------------------------------------------

newtype SpecVersion = SpecVersion
  { unSpecVersion :: Integer }


instance FromJSON SpecVersion where
  parseJSON n@(YAML.Number _) = SpecVersion <$> parseJSON n
  parseJSON invalid           = YAML.typeMismatch "SpecVersion" invalid


-- TYPE
--------------------------------------------------------------------------------

-- ** Type
--------------------------------------------------------------------------------

data Type = 
    Product ProductType
  | Sum SumType


instance FromJSON Type where
  parseJSON obj@(YAML.Object m) = do
    typeString <- m .: "type" :: Parser Text
    case typeString of
      "product" -> Product <$> parseJSON obj
      "sum"     -> Sum <$> parseJSON obj
  parseJSON invalid    = YAML.typeMismatch "Type" invalid 


-- ** General Type Data
--------------------------------------------------------------------------------

data GeneralTypeData = GeneralTypeData
  { typeName        :: Text
  , typeDescription :: Maybe Text
  }


instance FromJSON GeneralTypeData where
  parseJSON (YAML.Object m) = GeneralTypeData
                          <$> m .:  "name" 
                          <*> m .:? "description" 
  parseJSON invalid    = YAML.typeMismatch "GeneralTypeData" invalid


-- ** Presence
--------------------------------------------------------------------------------

data Presence = Optional | Required

instance FromJSON Presence where
  parseJSON (YAML.String s) = case s of
                                "optional" -> return Optional
                                "required" -> return Required
  parseJSON invalid     = YAML.typeMismatch "GeneralTypeData" invalid


-- > Product Type
--------------------------------------------------------------------------------

-- ** Product Type
--------------------------------------------------------------------------------

data ProductType = ProductType
  { productGeneralData :: GeneralTypeData 
  , fields             :: [Field]
  }


instance FromJSON ProductType where
  parseJSON obj@(YAML.Object hm) = ProductType 
                               <$> parseJSON obj
                               <*> hm .: "fields"
  parseJSON invalid         = YAML.typeMismatch "GeneralTypeData" invalid


-- ** Field
--------------------------------------------------------------------------------

data Field = Field
  { fieldName :: FieldName
  , presence  :: Presence
  , fieldType :: ValueType  
  }


instance FromJSON Field where
  parseJSON obj@(YAML.Object m) = Field 
                              <$> m .: "name"
                              <*> m .: "presence"
                              <*> parseJSON obj
  parseJSON invalid         = YAML.typeMismatch "GeneralTypeData" invalid


-- ** Field Name
--------------------------------------------------------------------------------

newtype FieldName = FieldName
  { unFieldName :: Text }


instance FromJSON FieldName where
  parseJSON (YAML.String s) = return $ FieldName s
  parseJSON invalid         = YAML.typeMismatch "GeneralTypeData" invalid


-- > Sum Type
--------------------------------------------------------------------------------

-- ** Sum Type
--------------------------------------------------------------------------------

data SumType = SumType
  { sumGeneralData :: GeneralTypeData 
  , cases       :: [Case]
  }


instance FromJSON SumType where
  parseJSON obj@(YAML.Object hm) = SumType 
                               <$> parseJSON obj
                               <*> hm .: "cases"
  parseJSON invalid         = YAML.typeMismatch "FieldName" invalid


-- ** Sum Type
--------------------------------------------------------------------------------

data Case = Case
  { caseName :: CaseName 
  , caseType :: ValueType  
  }


instance FromJSON Case where
  parseJSON obj@(YAML.Object m) = Case 
                              <$> m .: "name" 
                              <*> parseJSON obj
  parseJSON invalid             = YAML.typeMismatch "FieldName" invalid


-- ** Case Name
--------------------------------------------------------------------------------

newtype CaseName = CaseName
  { unCaseName :: Text }


instance FromJSON CaseName where
  parseJSON (YAML.String s) = return $ CaseName s
  parseJSON invalid         = YAML.typeMismatch "CaseName" invalid


-- VALUE TYPE
--------------------------------------------------------------------------------

-- ** Value Type
--------------------------------------------------------------------------------

data ValueType = 
    Custom CustomTypeName
  | PrimList PrimValueType
  | CustomTypeList CustomTypeName
  | Prim PrimValueType


data PrimValueType = 
    Any
  | Number
  | StringAlphaNumUnderscore
  | StringUtf8
  | Boolean


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


textToPrimType :: Text -> Maybe PrimValueType
textToPrimType typeText = 
  case typeText of
    "any"                        -> Just Any
    "number"                     -> Just Number
    "string_alphanum_underscore" -> Just StringAlphaNumUnderscore
    "string_utf8"                -> Just StringUtf8
    "boolean"                    -> Just Boolean
    _                            -> Nothing


listType :: Text -> ValueType
listType parameterType = 
  let mPrimType = textToPrimType parameterType
  in  case mPrimType of
        Just primType -> PrimList primType
        Nothing       -> CustomTypeList $ CustomTypeName parameterType


-- ** Custom Type Name
--------------------------------------------------------------------------------

newtype CustomTypeName = CustomTypeName
  { unCustomTypeName :: Text }


instance FromJSON CustomTypeName where
  parseJSON (YAML.String s) = return $ CustomTypeName s
  parseJSON invalid         = YAML.typeMismatch "CustomTypeName" invalid


-- CONSTRAINT
--------------------------------------------------------------------------------

-- ** Constraint
--------------------------------------------------------------------------------

data Constraint = 
    StringOneOf StringOneOfConstraint
  | NumGreaterThan NumberGreaterThanConstraint
    

instance FromJSON Constraint where
  parseJSON obj@(YAML.Object m) = do
    typeString <- m .: "type" :: Parser Text
    case typeString of
      "string_one_of"       -> StringOneOf <$> parseJSON obj
      "number_greater_than" -> NumGreaterThan <$> parseJSON obj
  parseJSON invalid         = YAML.typeMismatch "Constraint" invalid 


-- ** General Constraint Data
--------------------------------------------------------------------------------

data GeneralConstraintData = GeneralConstraintData
  { constraintName        :: ConstraintName
  , constraintDescription :: Maybe ConstraintDescription
  }


instance FromJSON GeneralConstraintData where
  parseJSON (YAML.Object m) = GeneralConstraintData
                          <$> m .:  "name" 
                          <*> m .:? "description" 
  parseJSON invalid         = YAML.typeMismatch "GeneralConstraintData" invalid 


-- ** Constraint Name
--------------------------------------------------------------------------------

newtype ConstraintName = ConstraintName
  { unConstraintName :: Text }


instance FromJSON ConstraintName where
  parseJSON (YAML.String s) = return $ ConstraintName s
  parseJSON invalid         = YAML.typeMismatch "ConstraintName" invalid


-- ** Constraint Description
--------------------------------------------------------------------------------

newtype ConstraintDescription = ConstraintDescription
  { unConstraintDescription :: Text }


instance FromJSON ConstraintDescription where
  parseJSON (YAML.String s) = return $ ConstraintDescription s
  parseJSON invalid         = YAML.typeMismatch "ConstraintDescription" invalid


-- > STRING ONE OF
--------------------------------------------------------------------------------

data StringOneOfConstraint = StringOneOfConstraint
  { stringOneOfInfo :: GeneralConstraintData
  , stringSet       :: HashSet String 
  }


instance FromJSON StringOneOfConstraint where
  parseJSON obj@(YAML.Object m) = StringOneOfConstraint
                              <$> parseJSON obj
                              <*> m .: "set" 
  parseJSON invalid         = YAML.typeMismatch "StringOneOfConstraint" invalid 


-- > GREATER THAN
--------------------------------------------------------------------------------

data NumberGreaterThanConstraint = NumberGreaterThanConstraint
  { numberGreaterThanInfo :: GeneralConstraintData
  , exclusiveLowerBound   :: Double
  }


instance FromJSON NumberGreaterThanConstraint where
  parseJSON obj@(YAML.Object m) = NumberGreaterThanConstraint
                              <$> parseJSON obj
                              <*> m .: "greater_than" 
  parseJSON invalid         = YAML.typeMismatch "NumberGreaterThanConstraint" invalid 

