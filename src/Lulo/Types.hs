
{-| Types
-}


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Lulo.Types where


import Control.Applicative ((<|>))
import Control.Lens
import Control.Lens.TH (makeFields)

import qualified Data.Aeson.Types as YAML
import Data.Foldable (foldl')
import Data.Hashable
import qualified Data.HashMap.Lazy as HML
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml ((.:), (.:?), Parser, FromJSON, parseJSON)
import qualified Data.Yaml as YAML

import GHC.Generics (Generic)



-- APPLICATION
--------------------------------------------------------------------------------

-- > PARAMETERS
--------------------------------------------------------------------------------

data Parameters = Parameters 
  { _parametersSpecFilename   :: FilePath 
  , _parametersVerbosity      :: Verbosity
  , _parametersHtmlFilename   :: Maybe FilePath
  , _parametersCssFilename    :: Maybe FilePath
  , _parametersHtmlFilePretty :: Bool
  }


data Verbosity = 
    Normal 
  | Verbose
  deriving (Eq)


-- SPECIFICATION
--------------------------------------------------------------------------------

data Spec = Spec
  { _specVersion         :: SpecVersion
  , _specTypes           :: [ObjectType]
  , _specConstraintIndex :: HashMap ConstraintName ValueConstraint
  }


instance FromJSON Spec where
  parseJSON (YAML.Object o) = do
    version <- o .: "version"
    typeList <- o .: "types"
    constList <- o .: "constraints" :: Parser [ValueConstraint] 
    let constIndex = newConstraintIndex constList
    return $ Spec version typeList constIndex 
  parseJSON invalid    = YAML.typeMismatch "Spec" invalid


newConstraintIndex :: [ValueConstraint] -> HashMap ConstraintName ValueConstraint
newConstraintIndex = foldl' indexConstraint HML.empty
  where
    indexConstraint hm c = 
      let cName = (_commonConstraintDataName $ _valueConstraintCommon c)
      in  HML.insert cName c hm


specConstraint :: Spec -> ConstraintName -> Maybe ValueConstraint
specConstraint spec constraintName =
      builtInConstraint constraintName
  <|> HML.lookup constraintName (_specConstraintIndex spec)


-- Specification > Version
--------------------------------------------------------------------------------

newtype SpecVersion = SpecVersion
  { unSpecVersion :: Integer }


instance FromJSON SpecVersion where
  parseJSON n@(YAML.Number _) = SpecVersion <$> parseJSON n
  parseJSON invalid           = YAML.typeMismatch "SpecVersion" invalid



-- OBJECT TYPE
--------------------------------------------------------------------------------

data ObjectType = ObjectType 
  { _objectTypeCommon   :: CommonTypeData
  , _objectTypeTypeData :: TypeData
  }


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


-- ObjectType > Type Data
--------------------------------------------------------------------------------

data TypeData = 
    Product ProductType
  | Sum SumType


-- Object Type > Common Type Data
--------------------------------------------------------------------------------

data CommonTypeData = CommonTypeData
  { _commonTypeDataName        :: Text
  , _commonTypeDataLabel       :: Text
  , _commonTypeDataDescription :: Maybe Text
  }


instance FromJSON CommonTypeData where
  parseJSON (YAML.Object m) = CommonTypeData
                          <$> m .:  "name" 
                          <*> m .:  "label" 
                          <*> m .:? "description" 
  parseJSON invalid    = YAML.typeMismatch "CommonTypeData" invalid


-- PRODUCT TYPE
--------------------------------------------------------------------------------

-- Product Type
--------------------------------------------------------------------------------

data ProductType = ProductType
  { _productTypeFields :: [Field]
  }


instance FromJSON ProductType where
  parseJSON obj@(YAML.Object hm) = ProductType 
                               <$> hm .: "fields"
  parseJSON invalid         = YAML.typeMismatch "ProductType" invalid


-- ProductType > Field
--------------------------------------------------------------------------------

data Field = Field
  { _fieldName         :: FieldName
  , _fieldPresence     :: Presence
  , _fieldDescription  :: Maybe FieldDescription
  , _fieldValueType    :: ValueType  
  , _fieldConstraints  :: [ConstraintName]
  , _fieldDefaultValue :: Maybe FieldDefaultValue
  }


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

newtype FieldName = FieldName
  { unFieldName :: Text }


instance FromJSON FieldName where
  parseJSON (YAML.String s) = return $ FieldName s
  parseJSON invalid         = YAML.typeMismatch "FieldName" invalid


-- ProductType > Field > Description
--------------------------------------------------------------------------------

newtype FieldDescription = FieldDescription
  { unFieldDesc :: Text }


instance FromJSON FieldDescription where
  parseJSON (YAML.String s) = return $ FieldDescription s
  parseJSON invalid         = YAML.typeMismatch "FieldDescription" invalid


-- ProductType > Field > Default Value
--------------------------------------------------------------------------------

newtype FieldDefaultValue = FieldDefaultValue
  { unFieldDefaultValue :: Text }


instance FromJSON FieldDefaultValue where
  parseJSON (YAML.String s) = return $ FieldDefaultValue s
  parseJSON invalid         = YAML.typeMismatch "FieldDefaultValue" invalid


-- ProductType > Field > Presence
--------------------------------------------------------------------------------

data Presence = 
    Optional 
  | Required
  deriving (Show)


instance FromJSON Presence where
  parseJSON (YAML.String s) = case s of
                                "optional" -> return Optional
                                "required" -> return Required
  parseJSON invalid     = YAML.typeMismatch "Presence" invalid


-- SUM TYPE
--------------------------------------------------------------------------------

-- Sum Type
--------------------------------------------------------------------------------

data SumType = SumType
  { _sumTypeCases  :: [SumCase]
  }


instance FromJSON SumType where
  parseJSON obj@(YAML.Object hm) = SumType 
                               <$> hm .: "cases"
  parseJSON invalid              = YAML.typeMismatch "SumType" invalid


-- ** Sum Type
--------------------------------------------------------------------------------

data SumCase = SumCase
  { _sumCaseName        :: SumCaseName 
  , _sumCaseDescription :: Maybe SumCaseDescription
  , _sumCaseValueType   :: ValueType  
  }


instance FromJSON SumCase where
  parseJSON obj@(YAML.Object hm) = SumCase 
                               <$> hm .: "name" 
                               <*> hm .:? "description" 
                               <*> parseJSON obj
  parseJSON invalid             = YAML.typeMismatch "SumCase" invalid


-- ** Case Name
--------------------------------------------------------------------------------

newtype SumCaseName = SumCaseName
  { unSumCaseName :: Text }


instance FromJSON SumCaseName where
  parseJSON (YAML.String s) = return $ SumCaseName s
  parseJSON invalid         = YAML.typeMismatch "SumCaseName" invalid


-- ** Sum Case Description
--------------------------------------------------------------------------------

newtype SumCaseDescription = SumCaseDescription
  { unSumCaseDesc :: Text }


instance FromJSON SumCaseDescription where
  parseJSON (YAML.String s) = return $ SumCaseDescription s
  parseJSON invalid         = YAML.typeMismatch "SumCaseDescription" invalid


-- VALUE TYPE
--------------------------------------------------------------------------------

-- ** Value Type
--------------------------------------------------------------------------------

data ValueType = 
    Custom CustomTypeName
  | PrimList PrimValueType
  | CustomTypeList CustomTypeName
  | Prim PrimValueType


instance Show ValueType where
  show (Custom (CustomTypeName typeName))         = T.unpack typeName
  show (PrimList primValueType)                   = 
    show primValueType ++ " list"
  show (CustomTypeList (CustomTypeName typeName)) = T.unpack typeName ++ " list"
  show (Prim primValueType)                       = show primValueType


data PrimValueType = 
    Any
  | Number
  | StringAlphaNumUnderscore
  | StringUtf8
  | Boolean


instance Show PrimValueType where
  show Any                      = "any"
  show Number                   = "number"
  show StringAlphaNumUnderscore = "alphanumeric string with underscores"
  show StringUtf8               = "UTF-8 string"
  show Boolean                  = "true/false"


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


-- CONSTRAINTS
--------------------------------------------------------------------------------

-- ** Value Constraint
--------------------------------------------------------------------------------

data ValueConstraint = ValueConstraint
  { _valueConstraintCommon     :: CommonConstraintData
  , _valueConstraintConstraint :: Constraint
  }


instance FromJSON ValueConstraint where
  parseJSON obj@(YAML.Object m) = ValueConstraint 
                              <$> parseJSON obj
                              <*> parseJSON obj
  parseJSON invalid    = YAML.typeMismatch "ValueConstraint" invalid 


-- ** Constraint
--------------------------------------------------------------------------------

data Constraint =
    StringOneOf StringOneOfConstraint
  | NumGreaterThan NumberGreaterThanConstraint


instance FromJSON Constraint where
  parseJSON (YAML.Object m) = do 
    typeString <- m .: "type" :: Parser Text
    paramtersValue <- m .: "parameters" :: Parser YAML.Value
    case typeString of
      "string_one_of"       -> StringOneOf <$> parseJSON paramtersValue
      "number_greater_than" -> NumGreaterThan <$> parseJSON paramtersValue
  parseJSON invalid         = YAML.typeMismatch "CommonConstraintData" invalid 

    
-- ** General Constraint Data
--------------------------------------------------------------------------------

data CommonConstraintData = CommonConstraintData
  { _commonConstraintDataName        :: ConstraintName
  , _commonConstraintDataDescription :: Maybe ConstraintDescription
  }


instance FromJSON CommonConstraintData where
  parseJSON (YAML.Object m) = CommonConstraintData
                          <$> m .:  "name" 
                          <*> m .:? "description" 
  parseJSON invalid         = YAML.typeMismatch "CommonConstraintData" invalid 


-- ** Constraint Name
--------------------------------------------------------------------------------

newtype ConstraintName = ConstraintName
  { unConstraintName :: Text }
  deriving (Eq, Generic)


instance Hashable ConstraintName


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
  { _stringOneOfConstraintSet :: HashSet Text
  }


instance FromJSON StringOneOfConstraint where
  parseJSON obj@(YAML.Object m) = StringOneOfConstraint
                              <$> m .: "set" 
  parseJSON invalid         = YAML.typeMismatch "StringOneOfConstraint" invalid 


-- > GREATER THAN
--------------------------------------------------------------------------------

-- Lower Bound is Exclusive
data NumberGreaterThanConstraint = NumberGreaterThanConstraint
  { _numberGreaterThanConstraintLowerBound :: Double
  }


instance FromJSON NumberGreaterThanConstraint where
  parseJSON obj@(YAML.Object m) = NumberGreaterThanConstraint
                              <$> m .: "greater_than" 
  parseJSON invalid         = YAML.typeMismatch "NumberGreaterThanConstraint" invalid 



-- > BUILT-IN CONSTRAINTS
--------------------------------------------------------------------------------

builtInConstraint :: ConstraintName -> Maybe ValueConstraint
builtInConstraint (ConstraintName constraintName) =
  case constraintName of
    "positive_integer" -> Just positiveIntegerConstraint
    _                  -> Nothing


positiveIntegerConstraint :: ValueConstraint
positiveIntegerConstraint = 
  ValueConstraint
    (CommonConstraintData 
      (ConstraintName $ T.pack "Positive Integer") 
      (Just $ ConstraintDescription $ T.pack "Any integer i > 0"))
    (NumGreaterThan $ NumberGreaterThanConstraint 0)


-- MAKE LENSES
--------------------------------------------------------------------------------

makeFields ''Parameters
makeFields ''Spec
makeFields ''ObjectType
makeFields ''CommonTypeData
makeFields ''ProductType
makeFields ''Field
makeFields ''SumType
makeFields ''SumCase
makeFields ''ValueConstraint
makeFields ''CommonConstraintData
makeFields ''StringOneOfConstraint
makeFields ''NumberGreaterThanConstraint
