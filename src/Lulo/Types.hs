
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

import Data.Foldable (foldl')
import Data.Hashable
import qualified Data.HashMap.Lazy as HML
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as YAML (Value)

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


-- OBJECT TYPE
--------------------------------------------------------------------------------

data ObjectType = ObjectType 
  { _objectTypeCommon   :: CommonTypeData
  , _objectTypeTypeData :: TypeData
  }

-- ObjectType > Type Data
--------------------------------------------------------------------------------

data TypeData = 
    Product ProductType
  | Sum SumType


-- Object Type > Common Type Data
--------------------------------------------------------------------------------

data CommonTypeData = CommonTypeData
  { _commonTypeDataName         :: Text
  , _commonTypeDataLabel        :: Text
  , _commonTypeDataDescription  :: Maybe Text
  , _commonTypeDataGroup        :: Maybe Text
  , _commonTypeDataYamlExamples :: [YAML.Value]
  }


groupToTypeMap :: [ObjectType] -> HashMap Text [ObjectType]
groupToTypeMap = foldl' indexObjectType HML.empty 
  where
    indexObjectType hm objectType = 
      let mGroupName = _commonTypeDataGroup $ _objectTypeCommon objectType
          groupName  = case mGroupName of
                         Just gName -> gName
                         Nothing    -> "no_group"
      in  HML.insertWith (++) groupName [objectType] hm


-- PRODUCT TYPE
--------------------------------------------------------------------------------

-- Product Type
--------------------------------------------------------------------------------

data ProductType = ProductType
  { _productTypeFields :: [Field]
  }


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


-- ProductType > Field > Name
--------------------------------------------------------------------------------

newtype FieldName = FieldName
  { unFieldName :: Text }


-- ProductType > Field > Description
--------------------------------------------------------------------------------

newtype FieldDescription = FieldDescription
  { unFieldDesc :: Text }



-- ProductType > Field > Default Value
--------------------------------------------------------------------------------

newtype FieldDefaultValue = FieldDefaultValue
  { unFieldDefaultValue :: Text }


-- ProductType > Field > Presence
--------------------------------------------------------------------------------

data Presence = 
    Optional 
  | Required
  deriving (Show)


-- SUM TYPE
--------------------------------------------------------------------------------

-- Sum Type
--------------------------------------------------------------------------------

data SumType = SumType
  { _sumTypeCases  :: [SumCase]
  }


-- SumType > SumCase
--------------------------------------------------------------------------------

data SumCase = SumCase
  { _sumCaseName        :: SumCaseName 
  , _sumCaseDescription :: Maybe SumCaseDescription
  , _sumCaseValueType   :: ValueType  
  }


-- SumType > SumCase > Name
--------------------------------------------------------------------------------

newtype SumCaseName = SumCaseName
  { unSumCaseName :: Text }


-- SumType > SumCase > Description
--------------------------------------------------------------------------------

newtype SumCaseDescription = SumCaseDescription
  { unSumCaseDesc :: Text }


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


-- ValueType > Custom Type Name
--------------------------------------------------------------------------------

newtype CustomTypeName = CustomTypeName
  { unCustomTypeName :: Text }


-- VALUE CONSTRAINT
--------------------------------------------------------------------------------

data ValueConstraint = ValueConstraint
  { _valueConstraintCommon     :: CommonConstraintData
  , _valueConstraintConstraint :: Constraint
  }
  

-- ValueConstraint > CommonConstraintData
--------------------------------------------------------------------------------

data CommonConstraintData = CommonConstraintData
  { _commonConstraintDataName        :: ConstraintName
  , _commonConstraintDataDescription :: Maybe ConstraintDescription
  }


-- ValueConstraint > Name
--------------------------------------------------------------------------------

newtype ConstraintName = ConstraintName
  { unConstraintName :: Text }
  deriving (Eq, Generic)


instance Hashable ConstraintName


-- ValueConstraint > Description
--------------------------------------------------------------------------------

newtype ConstraintDescription = ConstraintDescription
  { unConstraintDescription :: Text }


-- ValueConstraint > Constraint
--------------------------------------------------------------------------------

data Constraint =
    StringOneOf StringOneOfConstraint
  | NumGreaterThan NumberGreaterThanConstraint


-- Value Constraint > Constraint > String One Of
--------------------------------------------------------------------------------

data StringOneOfConstraint = StringOneOfConstraint
  { _stringOneOfConstraintSet :: HashSet Text
  }


-- Value Constraint > Constraint > Number Greater Than
--------------------------------------------------------------------------------

-- Lower Bound is Exclusive
data NumberGreaterThanConstraint = NumberGreaterThanConstraint
  { _numberGreaterThanConstraintLowerBound :: Double
  }


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
