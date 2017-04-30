
{-| Types
-}


module Lulo.Types where



-- SPECIFICATION
--------------------------------------------------------------------------------

data Spec = Spec
  { version :: SpecVersion
  , types   :: [Type]
  }



newtype SpecVersion = SpecVersion
  { unSpecVersion :: Integer  }


-- TYPE
--------------------------------------------------------------------------------

data Type = 
    Product ProductType
  | Sum SumType


data GeneralTypeData = GeneralTypeData
  { name        :: Text
  , description :: Text
  }


data Presence = Optional | Required


-- > Product Type
--------------------------------------------------------------------------------

data ProductType = ProductType
  { generalData : GeneralTypeData 
  , fields : [Field]
  }


data Field = Field
  { fieldName :: FieldName
  , presence  :: Presence
  , fieldType :: ValueType  
  }


newtype FieldName = FieldName
  { unFieldName :: Text }


-- > Sum Type
--------------------------------------------------------------------------------

data SumType = SumType
  { generalData :: GeneralTypeData 
  , cases       :: [Case]
  }


data Case = Case
  { caseName :: CaseName 
  , caseType :: ValueType  
  }


newtype CaseName = CaseName
  { unCaseName :: Text }


-- VALUE TYPE
--------------------------------------------------------------------------------

data ValueType = 
    CustomType CustomTypeName
  | CustomTypeList CustomTypeName
  | Any
  | Number
  | StringAlphaNumUnderscore
  | StringUtf8
  | Boolean
  | StringList


newtype CustomTypeName = CustomTypeName
  { unCustomTypeName :: Text }


-- CONSTRAINT
--------------------------------------------------------------------------------

data Constraint = 
    OneOfString OneOfStringConstraint
  | GreaterThan GreaterThanConstraint
    

data GeneralConstraintData = GeneralConstraintData
  { name :: ConstraintName
  }


-- One Of String
--------------------------------------------------------------------------------

data OneOfStringConstraint = OneOfStringConstraint
  { generalConstraintData :: GeneralConstraintData
  , stringSet             :: HashSet String 
  }


-- GreaterThan
--------------------------------------------------------------------------------

data GreaterThanConstraint = GreaterThanConstraint
  { generalConstraintData :: GeneralConstraintData
  , exclusiveLowerBound   :: Double
  }

newtype ConstraintName = ConstraintName
  { unConstraintName :: Text }
