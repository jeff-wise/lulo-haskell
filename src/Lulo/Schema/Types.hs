
{-| Types
-}


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.Schema.Types where


import Lulo.Doc.Parser
import Lulo.Doc.Types
import Lulo.Value

import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS (fromList)
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)



--------------------------------------------------------------------------------
-- SPECIFICATION
--------------------------------------------------------------------------------

data Schema = Schema
  { schemaVersion      :: SchemaVersion
  , schemaMetadata     :: SchemaMetadata
  , schemaDescription  :: Maybe SchemaDescription    
  , schemaRootTypeName :: CustomTypeName    
  , schemaTypes        :: [CustomType]
  , schemaConstraints  :: [Constraint]
  }


instance FromDocument Schema where
  fromDocument (DocDict doc) = Schema 
                          <$> (atParser "version" doc >>= fromDocument)
                          <*> (atParser "metadata" doc >>= fromDocument)
                          <*> (maybeAtParser "description" doc >>= fromMaybeDocument)
                          <*> (atParser "root_type" doc >>= fromDocument)
                          <*> (atListParser "types" doc >>= 
                            (\(ListDoc docs _ _) -> mapM customTypeFromDocument $ zip [0..] docs))
                          <*> (atMaybeListParser "constraints" doc >>= 
                                (\(ListDoc docs _ _) -> mapM fromDocument docs))
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


-- Specification > Version
--------------------------------------------------------------------------------

newtype SchemaVersion = SchemaVersion
  { getSchemaVersion :: Text }
  deriving (Eq, Show)


instance FromDocument SchemaVersion where
  fromDocument (DocText doc) = return $ SchemaVersion $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


-- Specification > Metadata
--------------------------------------------------------------------------------

data SchemaMetadata = SchemaMetadata
  { schemaName    :: SchemaName
  , schemaAuthors :: [SchemaAuthor]
  }


instance FromDocument SchemaMetadata where
  fromDocument (DocDict doc) = SchemaMetadata 
                           <$> (atParser "name" doc >>= fromDocument)
                           <*> (atListParser "authors" doc >>= 
                                  mapM fromDocument . listDocDocs)
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


-- Specification > Metadata > Name
--------------------------------------------------------------------------------

newtype SchemaName = SchemaName
  { getSchemaName :: Text }


instance FromDocument SchemaName where
  fromDocument (DocText doc) = return $ SchemaName $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)

-- Specification > Metadata > Author
--------------------------------------------------------------------------------

newtype SchemaAuthor = SchemaAuthor
  { schemaAuthorName :: Text 
  } deriving (Eq, Generic)


instance FromDocument SchemaAuthor where
  fromDocument (DocDict doc) = SchemaAuthor 
                           <$> atTextParser "name" doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


-- Specification > Description
--------------------------------------------------------------------------------

newtype SchemaDescription = SchemaDescription
  { descOverviewMarkdown :: Text
  } deriving (Eq, Generic)


instance FromDocument SchemaDescription where
  fromDocument (DocDict doc) = SchemaDescription 
                           <$> atTextParser "overview" doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


--------------------------------------------------------------------------------
-- CUSTOM TYPE
--------------------------------------------------------------------------------

data CustomType = 
    CustomTypeProduct ProductCustomType
  | CustomTypeSum     SumCustomType
  | CustomTypePrim    PrimCustomType
  | CustomTypeSymbol  SymbolCustomType
  deriving (Eq, Generic)


instance Hashable CustomType


customTypeFromDocument :: (Int, Doc) -> ValueParser CustomType
customTypeFromDocument (index, doc) =
  case docCase doc of
    "product_type"   -> CustomTypeProduct <$> productTypeFromDocument index doc
    "sum_type"       -> CustomTypeSum <$> sumTypeFromDocument index doc
    "primitive_type" -> CustomTypePrim <$> synonymTypeFromDocument index doc
    "symbol_type"    -> CustomTypeSymbol <$> symbolTypeFromDocument index doc
    _                -> Left $ ValueParseErrorUnknownCase $ 
        UnknownCaseError (docCase doc) "CustomType" (docPath doc)


instance FromDocument CustomType where
  fromDocument doc = 
    case docCase doc of
      "product_type"   -> CustomTypeProduct <$> fromDocument doc
      "sum_type"       -> CustomTypeSum <$> fromDocument doc
      "primitive_type" -> CustomTypePrim <$> fromDocument doc
      _                -> Left $ ValueParseErrorUnknownCase $ 
        UnknownCaseError (docCase doc) "CustomType" (docPath doc)


customTypeLabel :: CustomType -> String
customTypeLabel (CustomTypeProduct _) = "product"
customTypeLabel (CustomTypeSum     _) = "sum"
customTypeLabel (CustomTypePrim    _) = "primitive"
customTypeLabel (CustomTypeSymbol  _) = "symbol"


typeName :: CustomType -> CustomTypeName
typeName (CustomTypeProduct productType) = prodTypeName productType
typeName (CustomTypeSum     sumType    ) = sumTypeName sumType
typeName (CustomTypePrim    primType   ) = primTypeName primType
typeName (CustomTypeSymbol  symbolType ) = symTypeName symbolType


typeLabel :: CustomType -> CustomTypeLabel
typeLabel (CustomTypeProduct productType) = prodTypeLabel productType
typeLabel (CustomTypeSum     sumType    ) = sumTypeLabel sumType
typeLabel (CustomTypePrim    primType   ) = primTypeLabel primType
typeLabel (CustomTypeSymbol  symbolType ) = symTypeLabel symbolType


typeDescription :: CustomType -> Maybe CustomTypeDescription
typeDescription (CustomTypeProduct productType) = prodTypeDescription productType
typeDescription (CustomTypeSum     sumType    ) = sumTypeDescription sumType
typeDescription (CustomTypePrim    primType   ) = primTypeDescription primType
typeDescription (CustomTypeSymbol  symbolType ) = symTypeDescription symbolType


typeGroup :: CustomType -> Maybe CustomTypeGroup
typeGroup (CustomTypeProduct productType) = prodTypeGroup productType
typeGroup (CustomTypeSum     sumType    ) = sumTypeGroup sumType
typeGroup (CustomTypePrim    primType   ) = primTypeGroup primType
typeGroup (CustomTypeSymbol  symbolType ) = symTypeGroup symbolType


typeCodeExamples :: CustomType -> [CodeExample]
typeCodeExamples (CustomTypeProduct productType) = prodTypeCodeExamples productType
typeCodeExamples (CustomTypeSum     sumType    ) = sumTypeCodeExamples sumType
typeCodeExamples (CustomTypePrim    primType   ) = primTypeCodeExamples primType
typeCodeExamples (CustomTypeSymbol  symbolType ) = symTypeCodeExamples symbolType


typeOrder :: CustomType -> Int
typeOrder (CustomTypeProduct productType) = prodTypeOrder productType
typeOrder (CustomTypeSum     sumType    ) = sumTypeOrder sumType
typeOrder (CustomTypePrim    primType   ) = primTypeOrder primType
typeOrder (CustomTypeSymbol  symbolType ) = symTypeOrder symbolType


-- Custom Type > Data > Name
--------------------------------------------------------------------------------

newtype CustomTypeName = CustomTypeName
  { getCustomTypeName :: Text }
  deriving (Eq, Generic, Ord)


instance Hashable CustomTypeName


instance FromDocument CustomTypeName where
  fromDocument (DocText doc) = return $ CustomTypeName $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


-- Custom Type > Data > Label
--------------------------------------------------------------------------------

newtype CustomTypeLabel = CustomTypeLabel
  { getCustomTypeLabel :: Text }
  deriving (Eq, Generic)


instance Hashable CustomTypeLabel


instance FromDocument CustomTypeLabel where
  fromDocument (DocText doc) = return $ CustomTypeLabel $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


-- Custom Type > Data > Description
--------------------------------------------------------------------------------

newtype CustomTypeDescription = CustomTypeDescription
  { getCustomTypeDescription :: Text }
  deriving (Eq, Generic)


instance Hashable CustomTypeDescription


instance FromDocument CustomTypeDescription where
  fromDocument (DocText doc) = return $ CustomTypeDescription $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


-- Custom Type > Data > Group
--------------------------------------------------------------------------------

newtype CustomTypeGroup = CustomTypeGroup
  { getCustomTypeGroup :: Text }
  deriving (Eq, Generic, Ord, Show)


instance Hashable CustomTypeGroup


instance FromDocument CustomTypeGroup where
  fromDocument (DocText doc) = return $ CustomTypeGroup $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


--------------------------------------------------------------------------------
-- CUSTOM TYPE > PRODUCT
--------------------------------------------------------------------------------

data ProductCustomType = ProductCustomType
  { prodTypeName         :: CustomTypeName
  , prodTypeLabel        :: CustomTypeLabel
  , prodTypeDescription  :: Maybe CustomTypeDescription
  , prodTypeGroup        :: Maybe CustomTypeGroup
  -- , prodTypeYamlExamples :: [YAML.Value]
  , prodTypeFields       :: [Field]
  , prodTypeCodeExamples :: [CodeExample]
  , prodTypeOrder        :: Int
  } deriving (Eq, Generic)


instance Hashable ProductCustomType


productTypeFromDocument :: Int -> Doc -> ValueParser ProductCustomType
productTypeFromDocument index doc = do 
  customType <- fromDocument doc 
  let customTypeWithIndex = customType { prodTypeOrder = index }
  return customTypeWithIndex


instance FromDocument ProductCustomType where
  fromDocument (DocDict doc) = ProductCustomType 
                           <$> (atParser "name" doc >>= fromDocument)
                           <*> (atParser "label" doc >>= fromDocument)
                           <*> (maybeAtParser "description" doc >>= fromMaybeDocument)
                           <*> (maybeAtParser "group" doc >>= fromMaybeDocument)
                           -- <*> (atListParser "yaml_examples" doc >>= 
                           --       (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> (atListParser "fields" doc >>= 
                                 (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> (atMaybeListParser "code_examples" doc >>= 
                                 (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> return 0
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


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


instance FromDocument Field where
  fromDocument (DocDict doc) = Field 
                           <$> (atParser "name" doc >>= fromDocument)
                           <*> (atParser "presence" doc >>= fromDocument)
                           <*> (maybeAtParser "description" doc >>= fromMaybeDocument)
                           <*> (atParser "type" doc >>= fromDocument)
                           <*> return []
                           -- <*> (atListParser "constraints" doc >>= 
                           --       (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> (maybeAtParser "default_value" doc >>= fromMaybeDocument)
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


-- Custom Type > Product > Field > Name
--------------------------------------------------------------------------------

newtype FieldName = FieldName
  { getFieldName :: Text }
  deriving (Eq, Generic)


instance Hashable FieldName


instance FromDocument FieldName where
  fromDocument (DocText doc) = return $ FieldName $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


-- Custom Type > Product > Field > Description
--------------------------------------------------------------------------------

newtype FieldDescription = FieldDescription
  { getFieldDesc :: Text }
  deriving (Eq, Generic)


instance Hashable FieldDescription


instance FromDocument FieldDescription where
  fromDocument (DocText doc) = return $ FieldDescription $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


-- Custom Type > Product > Field > Default Value
--------------------------------------------------------------------------------

newtype FieldDefaultValue = FieldDefaultValue
  { getFieldDefaultValue :: Text }
  deriving (Eq, Generic)


instance Hashable FieldDefaultValue


instance FromDocument FieldDefaultValue where
  fromDocument (DocText doc) = return $ FieldDefaultValue $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


-- Custom Type > Product > Field > Presence
--------------------------------------------------------------------------------

data FieldPresence = 
    Optional 
  | Required
  deriving (Eq, Generic)


instance Show FieldPresence where
  show Optional = "Optional"
  show Required = "Required"


instance Hashable FieldPresence


instance FromDocument FieldPresence where
  fromDocument doc@(DocText textDoc) = 
    case textDocValue textDoc of
      "optional" -> return Optional
      "required" -> return Required
      _          -> Left $ ValueParseErrorUnexpectedValue $ 
        UnexpectedValueError "presence" (textDocValue textDoc) (docPath doc)
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


--------------------------------------------------------------------------------
-- CUSTOM TYPE > SUM
--------------------------------------------------------------------------------

data SumCustomType = SumCustomType
  { sumTypeName         :: CustomTypeName
  , sumTypeLabel        :: CustomTypeLabel
  , sumTypeDescription  :: Maybe CustomTypeDescription
  , sumTypeGroup        :: Maybe CustomTypeGroup
  -- , sumTypeYamlExamples :: [YAML.Value]
  , sumTypeCases        :: [Case]
  , sumTypeCodeExamples :: [CodeExample]
  , sumTypeOrder        :: Int
  } deriving (Eq, Generic)


instance Hashable SumCustomType


sumTypeFromDocument :: Int -> Doc -> ValueParser SumCustomType
sumTypeFromDocument index doc = do 
  customType <- fromDocument doc 
  let customTypeWithIndex = customType { sumTypeOrder = index }
  return customTypeWithIndex



instance FromDocument SumCustomType where
  fromDocument (DocDict doc) = SumCustomType 
                           <$> (atParser "name" doc >>= fromDocument)
                           <*> (atParser "label" doc >>= fromDocument)
                           <*> (maybeAtParser "description" doc >>= fromMaybeDocument)
                           <*> (maybeAtParser "group" doc >>= fromMaybeDocument)
                           -- <*> (atListParser "yaml_examples" doc >>= 
                           --       (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> (atListParser "cases" doc >>= 
                                 (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> (atMaybeListParser "code_examples" doc >>= 
                                 (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> return 0
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


sumTypeHasCase :: CustomTypeName -> SumCustomType -> Bool
sumTypeHasCase _typeName sumType = any caseHasType $ sumTypeCases sumType
  where
    caseHasType :: Case -> Bool
    caseHasType _case = caseType _case == _typeName


-- Custom Type > Sum > Case
--------------------------------------------------------------------------------

data Case = Case
  { caseType        :: CustomTypeName
  , caseDescription :: Maybe CaseDescription
  } deriving (Eq, Generic)


instance Hashable Case


instance FromDocument Case where
  fromDocument (DocDict doc) = Case 
                          <$> (atParser "type" doc >>= fromDocument)
                          <*> (maybeAtParser "description" doc >>= fromMaybeDocument)
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


-- Custom Type > Sum > Case > Description
--------------------------------------------------------------------------------

newtype CaseDescription = CaseDescription
  { getCaseDescription :: Text }
  deriving (Eq, Generic)


instance Hashable CaseDescription


instance FromDocument CaseDescription where
  fromDocument (DocText doc) = return $ CaseDescription $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


--------------------------------------------------------------------------------
-- CUSTOM TYPE > PRIMITIVE
--------------------------------------------------------------------------------

data PrimCustomType = PrimCustomType
  { primTypeName         :: CustomTypeName
  , primTypeLabel        :: CustomTypeLabel
  , primTypeDescription  :: Maybe CustomTypeDescription
  , primTypeGroup        :: Maybe CustomTypeGroup
  , primTypeBaseType     :: BaseType
  , primTypeConstraints  :: [ConstraintName] 
  , primTypeCodeExamples :: [CodeExample] 
  , primTypeOrder        :: Int
  } deriving (Eq, Generic)


instance Hashable PrimCustomType


synonymTypeFromDocument :: Int -> Doc -> ValueParser PrimCustomType
synonymTypeFromDocument index doc = do 
  customType <- fromDocument doc 
  let customTypeWithIndex = customType { primTypeOrder = index }
  return customTypeWithIndex


instance FromDocument PrimCustomType where
  fromDocument (DocDict doc) = PrimCustomType 
                           <$> (atParser "name" doc >>= fromDocument)
                           <*> (atParser "label" doc >>= fromDocument)
                           <*> (maybeAtParser "description" doc >>= fromMaybeDocument)
                           <*> (maybeAtParser "group" doc >>= fromMaybeDocument)
                           -- <*> (atListParser "yaml_examples" doc >>= 
                           --       (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> (atParser "base_type" doc >>= fromDocument)
                           <*> (atMaybeListParser "constraints" doc >>= 
                                 (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> (atMaybeListParser "code_examples" doc >>= 
                                 (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> return 0
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)



data BaseType = 
    BaseTypePrim PrimValueType
  | BaseTypeCustom CustomTypeName
  deriving (Eq, Generic)


instance Hashable BaseType


instance FromDocument BaseType where
  fromDocument doc = 
    case docCase doc of
      "prim_type"   -> BaseTypePrim <$> fromDocument doc
      "custom_type" -> BaseTypeCustom <$> fromDocument doc
      _                  -> Left $ ValueParseErrorUnknownCase $ 
        UnknownCaseError (docCase doc) "BaseType" (docPath doc)


baseTypeName :: BaseType -> String
baseTypeName (BaseTypePrim   primValueType)  = show primValueType
baseTypeName (BaseTypeCustom customTypeName) = 
  T.unpack $ getCustomTypeName customTypeName


--------------------------------------------------------------------------------
-- CUSTOM TYPE > SYMBOL
--------------------------------------------------------------------------------

data SymbolCustomType = SymbolCustomType
  { symTypeName         :: CustomTypeName
  , symTypeLabel        :: CustomTypeLabel
  , symTypeDescription  :: Maybe CustomTypeDescription
  , symTypeGroup        :: Maybe CustomTypeGroup
  , symTypeSymbol       :: Text
  , symTypeCodeExamples :: [CodeExample]
  , symTypeOrder        :: Int
  } deriving (Eq, Generic)


instance Hashable SymbolCustomType


symbolTypeFromDocument :: Int -> Doc -> ValueParser SymbolCustomType
symbolTypeFromDocument index doc = do 
  customType <- fromDocument doc 
  let customTypeWithIndex = customType { symTypeOrder = index }
  return customTypeWithIndex


instance FromDocument SymbolCustomType where
  fromDocument (DocDict doc) = SymbolCustomType 
                           <$> (atParser "name" doc >>= fromDocument)
                           <*> (atParser "label" doc >>= fromDocument)
                           <*> (maybeAtParser "description" doc >>= fromMaybeDocument)
                           <*> (maybeAtParser "group" doc >>= fromMaybeDocument)
                           <*> atTextParser "symbol" doc
                           <*> (atMaybeListParser "code_examples" doc >>= 
                                 (\(ListDoc docs _ _) -> mapM fromDocument docs))
                           <*> return 0
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


--------------------------------------------------------------------------------
-- CODE EXAMPLE
--------------------------------------------------------------------------------

data CodeExample = CodeExample
  { codeExampleLanguage    :: Text
  , codeExampleCase        :: Maybe Text
  , codeExampleCode        :: Text
  , codeExampleTitle       :: Text
  , codeExampleDescription :: Maybe Text
  } deriving (Eq, Generic)


instance Hashable CodeExample


instance FromDocument CodeExample where
  fromDocument (DocDict doc) = CodeExample 
                           <$> atTextParser "language" doc
                           <*> atMaybeTextParser "case" doc
                           <*> atTextParser "code" doc
                           <*> atTextParser "title" doc
                           <*> atMaybeTextParser "description" doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)





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


instance FromDocument ValueType where
  fromDocument doc = 
    case docCase doc of
      "prim_type"        -> Prim <$> fromDocument doc
      "prim_coll_type"   -> PrimList <$> fromDocument doc
      "custom_type"      -> Custom <$> fromDocument doc
      "custom_coll_type" -> CustomList <$> fromDocument doc
      _                  -> Left $ ValueParseErrorUnknownCase $ 
        UnknownCaseError (docCase doc) "ValueType" (docPath doc)


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


instance FromDocument PrimValueType where
  fromDocument doc@(DocText textDoc) = 
    case textDocValue textDoc of
      "any"     -> return Any
      "number"  -> return Number
      "string"  -> return String
      "boolean" -> return Boolean
      _          -> Left $ ValueParseErrorUnexpectedValue $ 
        UnexpectedValueError "prim_value_type" (textDocValue textDoc) (docPath doc)
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


  -- fromDocument doc = 
  --   case docCase doc of
  --     "any"     -> return Any
  --     "number"  -> return Number
  --     "string"  -> return String
  --     "boolean" -> return Boolean
  --     _         -> Left $ ValueParseErrorUnknownCase $ 
  --       UnknownCaseError (docCase doc) "PrimValueType" (docPath doc)


instance Show PrimValueType where
  show Any      = "Any"
  show Number   = "Number"
  show String   = "String"
  show Boolean  = "Boolean"


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

data Constraint =
    ConstraintStringOneOf StringOneOfConstraint
  | ConstraintNumGreaterThan NumGreaterThanConstraint
  deriving (Eq, Generic)
 

instance Hashable Constraint


instance FromDocument Constraint where
  fromDocument doc = 
    case docCase doc of
      "constraint_string_one_of"    -> ConstraintStringOneOf <$> fromDocument doc
      "constraint_num_greater_than" -> ConstraintNumGreaterThan <$> fromDocument doc
      _         -> Left $ ValueParseErrorUnknownCase $ 
        UnknownCaseError (docCase doc) "Constraint" (docPath doc)


constraintName :: Constraint -> ConstraintName
constraintName (ConstraintStringOneOf    c) = stringOneOfName c
constraintName (ConstraintNumGreaterThan c) = numGreaterThanName c 


constraintLabel :: Constraint -> ConstraintLabel
constraintLabel (ConstraintStringOneOf    c) = stringOneOfLabel c
constraintLabel (ConstraintNumGreaterThan c) = numGreaterThanLabel c 


-- Constraint > Data > Name
--------------------------------------------------------------------------------

newtype ConstraintName = ConstraintName
  { getConstraintName :: Text }
  deriving (Eq, Generic, Ord)


instance Hashable ConstraintName


instance FromDocument ConstraintName where
  fromDocument (DocText doc) = return $ ConstraintName $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


-- Constraint > Data > Label
--------------------------------------------------------------------------------

newtype ConstraintLabel = ConstraintLabel
  { getConstraintLabel :: Text }
  deriving (Eq, Generic)


instance Hashable ConstraintLabel


instance FromDocument ConstraintLabel where
  fromDocument (DocText doc) = return $ ConstraintLabel $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


-- Constraint > Data > Description
--------------------------------------------------------------------------------

newtype ConstraintDescription = ConstraintDescription
  { getConstraintDescription :: Text }
  deriving (Eq, Generic)


instance Hashable ConstraintDescription


instance FromDocument ConstraintDescription where
  fromDocument (DocText doc) = return $ ConstraintDescription $ textDocValue doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocTextType (docType doc) (docPath doc)


-- Constraint > String One Of
--------------------------------------------------------------------------------

data StringOneOfConstraint = StringOneOfConstraint
  { stringOneOfName        :: ConstraintName
  , stringOneOfLabel       :: ConstraintLabel
  , stringOneOfDescription :: ConstraintDescription
  , stringOneOfSet         :: HashSet StringOneOfValue
  } deriving (Eq, Generic)


instance Hashable StringOneOfConstraint


instance FromDocument StringOneOfConstraint where
  fromDocument (DocDict doc) = StringOneOfConstraint 
                           <$> (atParser "name" doc >>= fromDocument)
                           <*> (atParser "label" doc >>= fromDocument)
                           <*> (atParser "description" doc >>= fromDocument)
                           <*> (atListParser "set" doc >>= 
                                 (\(ListDoc docs _ _) -> HS.fromList <$> mapM fromDocument docs))
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


data StringOneOfValue = StringOneOfValue
  { stringOneOfValueText        :: Text 
  , stringOneOfValueDescription :: Maybe Text
  } deriving (Eq, Generic)


instance Hashable StringOneOfValue


instance FromDocument StringOneOfValue where
  fromDocument (DocDict doc) = StringOneOfValue 
                           <$> atTextParser "value" doc
                           <*> atMaybeTextParser "description" doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)




-- Constraint > Number Greater Than
--------------------------------------------------------------------------------

-- Lower Bound is Exclusive
data NumGreaterThanConstraint = NumGreaterThanConstraint
  { numGreaterThanName          :: ConstraintName
  , numGreaterThanLabel         :: ConstraintLabel
  , numGreaterThanDescription   :: ConstraintDescription
  , numberGreaterThanLowerBound :: Double
  } deriving (Eq, Generic)


instance Hashable NumGreaterThanConstraint


instance FromDocument NumGreaterThanConstraint where
  fromDocument (DocDict doc) = NumGreaterThanConstraint 
                           <$> (atParser "name" doc >>= fromDocument)
                           <*> (atParser "label" doc >>= fromDocument)
                           <*> (atParser "description" doc >>= fromDocument)
                           <*> atDoubleParser "lower_bound" doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)



