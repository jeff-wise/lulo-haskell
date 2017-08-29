
{-| Types
-}


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.Spec.Types where


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
  { specVersion      :: SpecVersion
  , specMetadata     :: SpecMetadata
  , specDescription  :: Maybe SpecDescription    
  , specRootTypeName :: Maybe CustomTypeName    
  , specTypes        :: [CustomType]
  , specConstraints  :: [Constraint]
  }


-- Specification > Version
--------------------------------------------------------------------------------

newtype SpecVersion = SpecVersion
  { getSpecVersion :: Text }
  deriving (Eq, Show)


-- Specification > Metadata
--------------------------------------------------------------------------------

data SpecMetadata = SpecMetadata
  { specName    :: SpecName
  , specAuthors :: [SpecAuthor]
  }


-- Specification > Metadata > Name
--------------------------------------------------------------------------------

newtype SpecName = SpecName
  { getSpecName :: Text }


-- Specification > Metadata > Author
--------------------------------------------------------------------------------

newtype SpecAuthor = SpecAuthor
  { getSpecAuthor :: Text }


-- Specification > Description
--------------------------------------------------------------------------------

newtype SpecDescription = SpecDescription
  { descOverviewMarkdown :: Text
  }


--------------------------------------------------------------------------------
-- CUSTOM TYPE
--------------------------------------------------------------------------------

data CustomType = CustomType 
  { typeData    :: CustomTypeData
  , customType' :: CustomType'
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


customTypeLabel :: CustomType' -> String
customTypeLabel (CustomTypeProduct _) = "product"
customTypeLabel (CustomTypeSum     _) = "sum"
customTypeLabel (CustomTypePrim    _) = "primitive"


-- Custom Type > Data
--------------------------------------------------------------------------------

data CustomTypeData = CustomTypeData
  { typeName         :: CustomTypeName
  , typeLabel        :: CustomTypeLabel
  , typeDescription  :: Maybe CustomTypeDescription
  , typeGroup        :: Maybe CustomTypeGroup
  , typeYamlExamples :: [YAML.Value]
  } deriving (Eq, Generic)


instance Hashable CustomTypeData


-- Custom Type > Data > Name
--------------------------------------------------------------------------------

newtype CustomTypeName = CustomTypeName
  { getCustomTypeName :: Text }
  deriving (Eq, Generic, Ord)


instance Hashable CustomTypeName


-- Custom Type > Data > Label
--------------------------------------------------------------------------------

newtype CustomTypeLabel = CustomTypeLabel
  { getCustomTypeLabel :: Text }
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
  deriving (Eq, Generic, Ord, Show)


instance Hashable CustomTypeGroup


--------------------------------------------------------------------------------
-- CUSTOM TYPE > PRODUCT
--------------------------------------------------------------------------------

newtype ProductCustomType = ProductCustomType
  { typeFields :: [Field]
  } deriving (Eq, Generic)


instance Hashable ProductCustomType


-- Custom Type > Product > Field
--------------------------------------------------------------------------------

data Field = Field
  { fieldName          :: FieldName
  , fieldPresence      :: FieldPresence
  , fieldDescription   :: Maybe FieldDescription
  , fieldValueType     :: ValueType
  , fieldConstraints   :: [ConstraintName]
  , fieldDefaultValue  :: Maybe FieldDefaultValue
  } deriving (Eq, Generic)


instance Hashable Field


-- Custom Type > Product > Field > Name
--------------------------------------------------------------------------------

newtype FieldName = FieldName
  { getFieldName :: Text }
  deriving (Eq, Generic)


instance Hashable FieldName


-- Custom Type > Product > Field > Description
--------------------------------------------------------------------------------

newtype FieldDescription = FieldDescription
  { getFieldDesc :: Text }
  deriving (Eq, Generic)


instance Hashable FieldDescription


-- Custom Type > Product > Field > Default Value
--------------------------------------------------------------------------------

newtype FieldDefaultValue = FieldDefaultValue
  { getFieldDefaultValue :: Text }
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
  { typeCases  :: [Case]
  } deriving (Eq, Generic)


instance Hashable SumCustomType


-- Custom Type > Sum > Case
--------------------------------------------------------------------------------

data Case = Case
  { caseType        :: CustomTypeName
  , caseDescription :: Maybe CaseDescription
  } deriving (Eq, Generic)


instance Hashable Case


-- Custom Type > Sum > Case > Description
--------------------------------------------------------------------------------

newtype CaseDescription = CaseDescription
  { getCaseDescription :: Text }
  deriving (Eq, Generic)


instance Hashable CaseDescription


--------------------------------------------------------------------------------
-- CUSTOM TYPE > PRIMITIVE
--------------------------------------------------------------------------------

data PrimCustomType = PrimCustomType
  { primTypeBaseType    :: PrimValueType
  , primTypeConstraints :: [ConstraintName] 
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
  show (Prim           primValueType)         = show primValueType
  show (PrimList       primValueType)         = 
    show primValueType ++ " list"
  show (Custom         (CustomTypeName name)) = T.unpack name
  show (CustomList     (CustomTypeName name)) = T.unpack name ++ " list"


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
  { constraintData :: ConstraintData
  , constraint'    :: Constraint'
  } deriving (Eq, Generic) 


instance Hashable Constraint


-- Constraint > Data 
--------------------------------------------------------------------------------

data ConstraintData = ConstraintData
  { constraintName        :: ConstraintName
  , constraintDescription :: Maybe ConstraintDescription
  } deriving (Eq, Generic)


instance Hashable ConstraintData


-- Constraint > Data > Name
--------------------------------------------------------------------------------

newtype ConstraintName = ConstraintName
  { getConstraintName :: Text }
  deriving (Eq, Generic, Ord)


instance Hashable ConstraintName


-- Constraint > Data > Description
--------------------------------------------------------------------------------

newtype ConstraintDescription = ConstraintDescription
  { getConstraintDescription :: Text }
  deriving (Eq, Generic)


instance Hashable ConstraintDescription


-- Constraint
--------------------------------------------------------------------------------

data Constraint' =
    StringOneOf    StringOneOfConstraint
  | NumGreaterThan NumberGreaterThanConstraint
  deriving (Eq, Generic)


instance Hashable Constraint'


constraintTypeString :: Constraint' -> String
constraintTypeString (StringOneOf    _) = "string_one_of"
constraintTypeString (NumGreaterThan _) = "number_greater_than"


-- Constraint > String One Of
--------------------------------------------------------------------------------

newtype StringOneOfConstraint = StringOneOfConstraint
  { stringOneOfSet :: HashSet Text
  } deriving (Eq, Generic)


instance Hashable StringOneOfConstraint


-- Constraint > Number Greater Than
--------------------------------------------------------------------------------

-- Lower Bound is Exclusive
newtype NumberGreaterThanConstraint = NumberGreaterThanConstraint
  { numberGreaterThanLowerBound :: Double
  } deriving (Eq, Generic)


instance Hashable NumberGreaterThanConstraint

