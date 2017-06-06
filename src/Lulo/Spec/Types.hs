
{-| Types
-}


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Lulo.Spec.Types where


import Control.Lens.TH (makeFields)

import Data.Hashable
import Data.HashSet (HashSet)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as YAML (Value)

import GHC.Generics (Generic)



--------------------------------------------------------------------------------
-- SPECIFICATION
--------------------------------------------------------------------------------

data Spec = Spec
  { _specVersion      :: SpecVersion
  , _specAuthors      :: [SpecAuthor]
  , _specDescription  :: Maybe SpecDescription    
  , _specRootTypeName :: Maybe CustomTypeName    
  , _specTypes        :: [CustomType]
  , _specConstraints  :: [Constraint]
  }


-- Specification > Version
--------------------------------------------------------------------------------

newtype SpecVersion = SpecVersion
  { getSpecVersion :: Text }
  deriving (Eq, Show)


-- Specification > Author
--------------------------------------------------------------------------------

newtype SpecAuthor = SpecAuthor
  { unSpecAuthor :: Text }


-- Specification > Description
--------------------------------------------------------------------------------

newtype SpecDescription = SpecDescription
  { _specDescriptionOverviewMarkdown :: Text
  }


--------------------------------------------------------------------------------
-- CUSTOM TYPE
--------------------------------------------------------------------------------

data CustomType = CustomType 
  { _customTypeTypeData   :: CustomTypeData
  , _customTypeCustomType :: CustomType'
  } deriving (Eq, Generic)


instance Hashable CustomType


-- Custom Type
--------------------------------------------------------------------------------

data CustomType' = 
    CustomTypeProduct ProductCustomType
  | CustomTypeSum     SumCustomType
  | CustomTypePrim    PrimCustomType
  deriving (Eq, Generic)


instance Hashable CustomType'


-- Custom Type > Data
--------------------------------------------------------------------------------

data CustomTypeData = CustomTypeData
  { _customTypeDataName         :: CustomTypeName
  , _customTypeDataLabel        :: CustomTypeLabel
  , _customTypeDataDescription  :: Maybe CustomTypeDescription
  , _customTypeDataGroup        :: Maybe CustomTypeGroup
  , _customTypeDataYamlExamples :: [YAML.Value]
  } deriving (Eq, Generic)


instance Hashable CustomTypeData


-- Custom Type > Data > Name
--------------------------------------------------------------------------------

newtype CustomTypeName = CustomTypeName
  { _customTypeNameText :: Text }
  deriving (Eq, Generic)


instance Hashable CustomTypeName


-- Custom Type > Data > Label
--------------------------------------------------------------------------------

newtype CustomTypeLabel = CustomTypeLabel
  { _customTypeLabelText :: Text }
  deriving (Eq, Generic)


instance Hashable CustomTypeLabel


-- Custom Type > Data > Description
--------------------------------------------------------------------------------

newtype CustomTypeDescription = CustomTypeDescription
  { getCustomTypeDescription :: Text }
  deriving (Eq, Generic)


instance Hashable CustomTypeDescription


-- Custom Type > Data > Group
--------------------------------------------------------------------------------

newtype CustomTypeGroup = CustomTypeGroup
  { getCustomTypeGroup :: Text }
  deriving (Eq, Generic, Show)


instance Hashable CustomTypeGroup


--------------------------------------------------------------------------------
-- CUSTOM TYPE > PRODUCT
--------------------------------------------------------------------------------

newtype ProductCustomType = ProductCustomType
  { _productCustomTypeFields :: [Field]
  } deriving (Eq, Generic)


instance Hashable ProductCustomType


-- Custom Type > Product > Field
--------------------------------------------------------------------------------

data Field = Field
  { _fieldName          :: FieldName
  , _fieldPresence      :: FieldPresence
  , _fieldDescription   :: Maybe FieldDescription
  , _fieldValueType     :: ValueType
  , _fieldConstraints   :: [ConstraintName]
  , _fieldDefaultValue  :: Maybe FieldDefaultValue
  } deriving (Eq, Generic)


instance Hashable Field


-- Custom Type > Product > Field > Name
--------------------------------------------------------------------------------

newtype FieldName = FieldName
  { unFieldName :: Text }
  deriving (Eq, Generic)


instance Hashable FieldName


-- Custom Type > Product > Field > Description
--------------------------------------------------------------------------------

newtype FieldDescription = FieldDescription
  { unFieldDesc :: Text }
  deriving (Eq, Generic)


instance Hashable FieldDescription


-- Custom Type > Product > Field > Default Value
--------------------------------------------------------------------------------

newtype FieldDefaultValue = FieldDefaultValue
  { unFieldDefaultValue :: Text }
  deriving (Eq, Generic)


instance Hashable FieldDefaultValue


-- Custom Type > Product > Field > Presence
--------------------------------------------------------------------------------

data FieldPresence = 
    Optional 
  | Required
  deriving (Eq, Generic, Show)


instance Hashable FieldPresence


--------------------------------------------------------------------------------
-- CUSTOM TYPE > SUM
--------------------------------------------------------------------------------

newtype SumCustomType = SumCustomType
  { _sumCustomTypeCases  :: [Case]
  } deriving (Eq, Generic)


instance Hashable SumCustomType


-- Custom Type > Sum > Case
--------------------------------------------------------------------------------

data Case = Case
  { _caseCaseType    :: CustomTypeName
  , _caseDescription :: Maybe CaseDescription
  } deriving (Eq, Generic)


instance Hashable Case


-- Custom Type > Sum > Case > Description
--------------------------------------------------------------------------------

newtype CaseDescription = CaseDescription
  { _caseDescriptionText :: Text }
  deriving (Eq, Generic)


instance Hashable CaseDescription


--------------------------------------------------------------------------------
-- CUSTOM TYPE > PRIMITIVE
--------------------------------------------------------------------------------

data PrimCustomType = PrimCustomType
  { _simpleCustomTypeBaseType    :: PrimValueType
  , _simpleCustomTypeConstraints :: [ConstraintName] 
  } deriving (Eq, Generic)


instance Hashable PrimCustomType


--------------------------------------------------------------------------------
-- VALUE TYPE
--------------------------------------------------------------------------------

data ValueType = 
    Prim PrimValueType
  | PrimList PrimValueType
  | Custom CustomTypeName
  | CustomList CustomTypeName
  deriving (Eq, Generic)


instance Hashable ValueType


instance Show ValueType where
  show (Prim           primValueType)             = show primValueType
  show (PrimList       primValueType)             = 
    show primValueType ++ " list"
  show (Custom         (CustomTypeName typeName)) = T.unpack typeName
  show (CustomList     (CustomTypeName typeName)) = 
    T.unpack typeName ++ " list"


-- Value > Primitive
--------------------------------------------------------------------------------

data PrimValueType = 
    Any
  | Number
  | String
  | Boolean
  deriving (Eq, Generic)


instance Hashable PrimValueType


instance Show PrimValueType where
  show Any      = "any"
  show Number   = "number"
  show String   = "string"
  show Boolean  = "true/false"


textToPrimType :: Text -> Maybe PrimValueType
textToPrimType typeText = 
  case typeText of
    "any"       -> Just Any
    "number"    -> Just Number
    "string"    -> Just String
    "boolean"   -> Just Boolean
    _           -> Nothing


listType :: Text -> ValueType
listType parameterType = 
  let mPrimType = textToPrimType parameterType
  in  case mPrimType of
        Just primType -> PrimList primType
        Nothing       -> CustomList $ CustomTypeName parameterType


--------------------------------------------------------------------------------
-- CONSTRAINT
--------------------------------------------------------------------------------

data Constraint = Constraint 
  { _constraintConstraintData :: ConstraintData
  , _constraintConstraint'    :: Constraint'
  } deriving (Eq, Generic) 


instance Hashable Constraint


-- Constraint > Data 
--------------------------------------------------------------------------------

data ConstraintData = ConstraintData
  { _constraintDataName        :: ConstraintName
  , _constraintDataDescription :: Maybe ConstraintDescription
  } deriving (Eq, Generic)


instance Hashable ConstraintData


-- Constraint > Data > Name
--------------------------------------------------------------------------------

newtype ConstraintName = ConstraintName
  { unConstraintName :: Text }
  deriving (Eq, Generic)


instance Hashable ConstraintName


-- Constraint > Data > Description
--------------------------------------------------------------------------------

newtype ConstraintDescription = ConstraintDescription
  { unConstraintDescription :: Text }
  deriving (Eq, Generic)


instance Hashable ConstraintDescription


-- Constraint
--------------------------------------------------------------------------------

data Constraint' =
    StringOneOf    StringOneOfConstraint
  | NumGreaterThan NumberGreaterThanConstraint
  deriving (Eq, Generic)


instance Hashable Constraint'


-- Constraint > String One Of
--------------------------------------------------------------------------------

newtype StringOneOfConstraint = StringOneOfConstraint
  { _stringOneOfConstraintStringSet :: HashSet Text
  } deriving (Eq, Generic)


instance Hashable StringOneOfConstraint


-- Constraint > Number Greater Than
--------------------------------------------------------------------------------

-- Lower Bound is Exclusive
newtype NumberGreaterThanConstraint = NumberGreaterThanConstraint
  { _numberGreaterThanConstraintLowerBound :: Double
  } deriving (Eq, Generic)


instance Hashable NumberGreaterThanConstraint


-- LENSES
--------------------------------------------------------------------------------

makeFields ''Spec
makeFields ''SpecDescription
makeFields ''CustomType
makeFields ''CustomTypeData
makeFields ''CustomTypeName
makeFields ''CustomTypeLabel
makeFields ''CustomTypeDescription
makeFields ''ProductCustomType
makeFields ''Field
makeFields ''SumCustomType
makeFields ''Case
makeFields ''CaseDescription
makeFields ''Constraint
makeFields ''ConstraintData
makeFields ''StringOneOfConstraint
makeFields ''NumberGreaterThanConstraint
