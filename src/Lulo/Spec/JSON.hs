
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


import Lulo.Spec.Types as Lulo

import Data.Aeson.Types (Pair, Series, typeMismatch, pairs)
import Data.Char (toLower)
import Data.Maybe (maybeToList)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml as Yaml
  -- ( (.:), (.:?)
  -- , Parser
  -- , FromJSON, ToJSON
  -- , parseJSON
  -- )



--------------------------------------------------------------------------------
-- SPECIFICATION
--------------------------------------------------------------------------------

instance FromJSON Spec where
  parseJSON (Object hm) = Spec
                      <$> hm .: "version"
                      <*> hm .: "metadata"
                      <*> hm .: "description"
                      <*> hm .: "root_type"
                      <*> hm .: "types"
                      <*> (concat . maybeToList <$> hm .:? "constraints")
  parseJSON invalid    = typeMismatch "Spec" invalid


instance ToJSON Spec where
  toJSON spec = 
    object [ "version"     .= specVersion spec 
           , "metadata"    .= specMetadata spec
           , "description" .= specDescription spec
           , "root_type"   .= specRootTypeName spec
           , "types"       .= specTypes spec
           , "constriants" .= specConstraints spec
           ]

  toEncoding spec =
    pairs ("version"     .= specVersion spec
        <> "metadata"    .= specMetadata spec
        <> "description" .= specDescription spec
        <> "root_type"   .= specRootTypeName spec
        <> "types"       .= specTypes spec
        <> "constriants" .= specConstraints spec
          )


-- Specification > Version
--------------------------------------------------------------------------------

instance FromJSON SpecVersion where
  parseJSON (Yaml.String s) = return $ SpecVersion s
  parseJSON invalid         = typeMismatch "SpecVersion" invalid


instance ToJSON SpecVersion where
  toJSON (SpecVersion version) = toJSON version
  toEncoding (SpecVersion version) = toEncoding version


-- Specification > Metadata
--------------------------------------------------------------------------------

instance FromJSON SpecMetadata where
  parseJSON (Object hm) = SpecMetadata
                      <$> hm .: "name"
                      <*> (concat . maybeToList <$> hm .:? "authors")
  parseJSON invalid     = typeMismatch "SpecMetadata" invalid


instance ToJSON SpecMetadata where
  toJSON metadata = 
    object [ "name"    .= specName metadata 
           , "authors" .= specAuthors metadata
           ]
  toEncoding metadata =
    pairs ("name"    .= specName metadata
        <> "authors" .= specAuthors metadata
          )


-- Specification > Metadata > Name
--------------------------------------------------------------------------------

instance FromJSON SpecName where
  parseJSON (Yaml.String s) = return $ SpecName s
  parseJSON invalid         = typeMismatch "SpecName" invalid


instance ToJSON SpecName where
  toJSON     (SpecName name) = toJSON name
  toEncoding (SpecName name) = toEncoding name


-- Specification > Metadata > Author
--------------------------------------------------------------------------------

instance FromJSON SpecAuthor where
  parseJSON (Object hm) = SpecAuthor <$> hm .: "name"
  parseJSON invalid     = typeMismatch "SpecAuthor" invalid


instance ToJSON SpecAuthor where
  toJSON (SpecAuthor name) = 
    object [ "name" .= name ]
  toEncoding (SpecAuthor name) =
    pairs ( "name" .= name )


-- Specification > Description
--------------------------------------------------------------------------------

instance FromJSON SpecDescription where
  parseJSON (Object hm) = SpecDescription <$> hm .: "overview_md"
  parseJSON invalid     = typeMismatch "SpecDescription" invalid


instance ToJSON SpecDescription where
  toJSON (SpecDescription overview) = 
    object [ "overview_md" .= overview ]
  toEncoding (SpecDescription overview) =
    pairs ( "overview_md" .= overview )


--------------------------------------------------------------------------------
-- CUSTOM TYPE
--------------------------------------------------------------------------------

instance FromJSON CustomType where
  parseJSON obj@(Object m) = do
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
  parseJSON invalid             = typeMismatch "CustomType" invalid 


instance ToJSON CustomType where
  toJSON (CustomType _typeData _customType') = object allPairs
    where
      typePair = ["type" .= (toJSON $ customTypeLabel _customType')]
      typeDataPairs = [ "name"          .= typeName _typeData
                      , "label"         .= typeLabel _typeData
                      , "description"   .= typeDescription _typeData
                      , "group"         .= typeGroup _typeData
                      , "examples_yaml" .= typeYamlExamples _typeData
                      ]
      allPairs = typePair ++ typeDataPairs ++ customTypePairs _customType'


customTypePairs :: CustomType' -> [Pair]
customTypePairs (CustomTypeProduct prod) = customTypeProductPairs prod
customTypePairs (CustomTypeSum     _sum) = customTypeSumPairs _sum
customTypePairs (CustomTypePrim    prim) = customTypePrimPairs prim


-- TODO
-- customTypeSeries :: CustomType' -> [Pair]
-- customTypeSeries (CustomTypeProduct prod) = customTypeProductPairs prod
-- customTypeSeries (CustomTypeSum     sum ) = customTypeSumPairs sum
-- customTypeSeries (CustomTypePrim    prim) = customTypePrimPairs prim


-- Custom Type > Data
--------------------------------------------------------------------------------

instance FromJSON CustomTypeData where
  parseJSON (Object m) = CustomTypeData
                     <$> m .:  "name" 
                     <*> m .:  "label" 
                     <*> m .:? "description" 
                     <*> m .:? "group" 
                     <*> (concat . maybeToList <$> m .:? "examples_yaml")
  parseJSON invalid    = typeMismatch "CustomTypeData" invalid


-- Custom Type > Data > Name
--------------------------------------------------------------------------------

instance FromJSON CustomTypeName where
  parseJSON (Yaml.String s) = return $ CustomTypeName s
  parseJSON invalid         = typeMismatch "CustomTypeName" invalid


instance ToJSON CustomTypeName where
  toJSON     (CustomTypeName name) = toJSON name
  toEncoding (CustomTypeName name) = toEncoding name


-- Custom Type > Data > Label
--------------------------------------------------------------------------------

instance FromJSON CustomTypeLabel where
  parseJSON (Yaml.String s) = return $ CustomTypeLabel s
  parseJSON invalid         = typeMismatch "CustomTypeLabel" invalid


instance ToJSON CustomTypeLabel where
  toJSON     (CustomTypeLabel label) = toJSON label
  toEncoding (CustomTypeLabel label) = toEncoding label


-- Custom Type > Data > Description
--------------------------------------------------------------------------------

instance FromJSON CustomTypeDescription where
  parseJSON (Yaml.String s) = return $ CustomTypeDescription s
  parseJSON invalid         = typeMismatch "CustomTypeDescription" invalid


instance ToJSON CustomTypeDescription where
  toJSON     (CustomTypeDescription desc) = toJSON desc
  toEncoding (CustomTypeDescription desc) = toEncoding desc


-- Custom Type > Data > Group
--------------------------------------------------------------------------------

instance FromJSON CustomTypeGroup where
  parseJSON (Yaml.String s) = return $ CustomTypeGroup s
  parseJSON invalid         = typeMismatch "CustomTypeGroup" invalid


instance ToJSON CustomTypeGroup where
  toJSON     (CustomTypeGroup group) = toJSON group
  toEncoding (CustomTypeGroup group) = toEncoding group


--------------------------------------------------------------------------------
-- CUSTOM TYPE > PRODUCT
--------------------------------------------------------------------------------

instance FromJSON ProductCustomType where
  parseJSON (Object hm) = ProductCustomType <$> hm .: "fields"
  parseJSON invalid     = typeMismatch "ProductCustomType" invalid


customTypeProductPairs :: ProductCustomType -> [Pair]
customTypeProductPairs productType = 
  ["fields" .= (toJSON $ typeFields productType) ]


-- Custom Type > Product > Field
--------------------------------------------------------------------------------

instance FromJSON Field where
  parseJSON obj@(Object m) = Field 
                         <$> m .: "name"
                         <*> m .: "presence"
                         <*> m .:? "description"
                         <*> parseJSON obj
                         <*> (concat . maybeToList <$> m .:? "constraints")
                         <*> m .:? "default_value"
  parseJSON invalid        = typeMismatch "Field" invalid


instance ToJSON Field where
  toJSON field = object ( [ "name"        .= fieldName field
                          , "presence"    .= fieldPresence field
                          , "description" .= fieldDescription field
                          , "value_type"  .= fieldValueType field
                          , "constraints" .= fieldConstraints field
                          ]
                         ++ (defaultValuePair $ fieldDefaultValue field))

  toEncoding field = pairs 
    ( "name"        .= fieldName field
   <> "presence"    .= fieldPresence field
   <> "description" .= fieldDescription field
   <> "constraints" .= fieldConstraints field
   <> "value_type"  .= fieldValueType field  
   <> (defaultValueSeries $ fieldDefaultValue field)
    )



defaultValuePair :: Maybe FieldDefaultValue -> [Pair]
defaultValuePair (Just defVal) = ["default_value" .= defVal]
defaultValuePair Nothing       = []

defaultValueSeries :: Maybe FieldDefaultValue -> Series
defaultValueSeries (Just defVal) = "default_value" .= defVal
defaultValueSeries Nothing       = mempty
 

-- Custom Type > Product > Field > Name
--------------------------------------------------------------------------------

instance FromJSON FieldName where
  parseJSON (Yaml.String s) = return $ FieldName s
  parseJSON invalid         = typeMismatch "FieldName" invalid


instance ToJSON FieldName where
  toJSON     (FieldName name) = toJSON name
  toEncoding (FieldName name) = toEncoding name


-- Custom Type > Product > Field > Description
--------------------------------------------------------------------------------

instance FromJSON FieldDescription where
  parseJSON (Yaml.String s) = return $ FieldDescription s
  parseJSON invalid         = typeMismatch "FieldDescription" invalid


instance ToJSON FieldDescription where
  toJSON     (FieldDescription desc) = toJSON desc
  toEncoding (FieldDescription desc) = toEncoding desc


-- Custom Type > Product > Field > Default Value
--------------------------------------------------------------------------------

instance FromJSON FieldDefaultValue where
  parseJSON (Yaml.String s) = return $ FieldDefaultValue s
  parseJSON invalid         = typeMismatch "FieldDefaultValue" invalid


instance ToJSON FieldDefaultValue where
  toJSON     (FieldDefaultValue defVal) = toJSON defVal
  toEncoding (FieldDefaultValue defVal) = toEncoding defVal


-- Custom Type > Product > Field > Presence
--------------------------------------------------------------------------------

instance FromJSON FieldPresence where
  parseJSON (Yaml.String s) = do
    let failMessage = "Presence must be either 'optional' or 'required'"
    case s of
      "optional" -> return Optional
      "required" -> return Required
      _          -> fail failMessage
  parseJSON invalid     = typeMismatch "Presence" invalid


instance ToJSON FieldPresence where
  toJSON     _fieldPresence = toJSON $ map toLower $ show _fieldPresence
  toEncoding _fieldPresence = toEncoding $ map toLower $ show _fieldPresence


--------------------------------------------------------------------------------
-- CUSTOM TYPE > SUM
--------------------------------------------------------------------------------

instance FromJSON SumCustomType where
  parseJSON (Object hm) = SumCustomType <$> hm .: "cases"
  parseJSON invalid     = typeMismatch "SumCustomType" invalid


-- instance ToJSON SumCustomType where
--   toJSON sumCustomType = 
--   toEncoding sumCustomType =
--     pairs ( "cases" .= typeCases sumCustomType )


customTypeSumPairs :: SumCustomType -> [Pair]
customTypeSumPairs sumType = [ "cases" .= (toJSON $ typeCases sumType) ]


-- Custom Type > Sum > Case
--------------------------------------------------------------------------------

instance FromJSON Case where
  parseJSON (Object hm) = Case 
                           <$> hm .: "type" 
                           <*> hm .:? "description" 
  parseJSON invalid     = typeMismatch "Case" invalid


instance ToJSON Case where
  toJSON _case = 
    object [ "type"        .= caseType _case 
           , "description" .= caseDescription _case
           ]
  toEncoding _case =
    pairs ( "type"        .= caseType _case
         <> "description" .= caseDescription _case)


-- Custom Type > Sum > Case > Description
--------------------------------------------------------------------------------

instance FromJSON CaseDescription where
  parseJSON (Yaml.String s) = return $ CaseDescription s
  parseJSON invalid         = typeMismatch "CaseDescription" invalid


instance ToJSON CaseDescription where
  toJSON     (CaseDescription desc) = toJSON desc
  toEncoding (CaseDescription desc) = toEncoding desc


--------------------------------------------------------------------------------
-- CUSTOM TYPE > PRIMITIVE
--------------------------------------------------------------------------------

instance FromJSON PrimCustomType where
  parseJSON (Object hm) = PrimCustomType 
                      <$> hm .: "base_type" 
                      <*> (concat . maybeToList <$> hm .:? "constraints")
  parseJSON invalid          = typeMismatch "PrimCustomType" invalid


-- instance ToJSON PrimCustomType where
--   toJSON primType = 
--     object [ "base_type"   .= primTypeBaseType primType
--            , "constraints" .= primTypeConstraints primType
--            ]
--   toEncoding primType =
--     pairs ( "base_type"   .= primTypeBaseType primType 
--          <> "constraints" .= primTypeConstraints primType
--           )


customTypePrimPairs :: PrimCustomType -> [Pair]
customTypePrimPairs primType =
    [ "base_type"   .= primTypeBaseType primType
    , "constraints" .= primTypeConstraints primType
    ]


--------------------------------------------------------------------------------
-- VALUE TYPE
--------------------------------------------------------------------------------

instance FromJSON ValueType where
  parseJSON (Object m) = do
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
  parseJSON invalid         = typeMismatch "ValueType" invalid


instance ToJSON ValueType where
  toJSON (Prim       primValueType ) = object [ "type" .= toJSON primValueType ]
  toJSON (PrimList   primValueType ) = object [ "type" .= toJSON ("list" :: String)
                                              , "of" .= toJSON primValueType ]
  toJSON (Custom     customTypeName) = object [ "type" .= toJSON customTypeName ]
  toJSON (CustomList customTypeName) = object [ "type" .= toJSON ("list" :: String)
                                              , "of" .= toJSON customTypeName ]

  -- toEncoding (Prim       primValueType ) = pairs ( "type" .= toEncoding primValueType )
  -- toEncoding (PrimList   primValueType ) = pairs ( "type" .= toEncoding ("list" :: String)
  --                                                <> "of" .= toEncoding primValueType )
  -- toEncoding (Custom     customTypeName) = pairs ( "type" .= toEncoding customTypeName )
  -- toEncoding (CustomList customTypeName) = pairs ( "type" .= toEncoding ("list" :: String)
  --                                                 <> "of" .= toEncoding customTypeName )


-- Value Type > Primitive
--------------------------------------------------------------------------------

instance FromJSON PrimValueType where
  parseJSON (Yaml.String s) = 
    case s of
      "any"     -> return Lulo.Any
      "number"  -> return Lulo.Number
      "string"  -> return Lulo.String
      "boolean" -> return Boolean
      other     -> fail $ "Unknown Primitive Value Type: " ++ T.unpack other
  parseJSON invalid         = typeMismatch "PrimValueType" invalid


instance ToJSON PrimValueType where
  toJSON Lulo.Any    = toJSON ("any" :: String)
  toJSON Lulo.Number = toJSON ("number" :: String)
  toJSON Lulo.String = toJSON ("string" :: String)
  toJSON Boolean     = toJSON ("boolean" :: String)

  toEncoding Lulo.Any     = toEncoding ("any" :: String)
  toEncoding Lulo.Number  = toEncoding ("number" :: String)
  toEncoding Lulo.String  = toEncoding ("string" :: String)
  toEncoding Boolean      = toEncoding ("boolean" :: String)


--------------------------------------------------------------------------------
-- CONSTRAINT
--------------------------------------------------------------------------------

instance FromJSON Constraint where
  parseJSON obj@(Object _) = Constraint 
                         <$> parseJSON obj
                         <*> parseJSON obj
  parseJSON invalid    = typeMismatch "Constraint" invalid 


instance ToJSON Constraint where
  toJSON _constraint = 
    object $ (constraintDataPairs $ constraintData _constraint)
          ++ (constraintPairs $ constraint' _constraint)
  toEncoding _constraint = 
    pairs $ (constraintDataSeries $ constraintData _constraint)
         <> (constraintSeries $ constraint' _constraint)


-- Constraint > Data
--------------------------------------------------------------------------------

instance FromJSON ConstraintData where
  parseJSON (Object hm) = ConstraintData
                      <$> hm .:  "name" 
                      <*> hm .:? "description" 
  parseJSON invalid     = typeMismatch "ConstraintData" invalid 


constraintDataPairs :: ConstraintData -> [Pair]
constraintDataPairs _constraintData =
  [ "name"        .= constraintName _constraintData 
  , "description" .= constraintDescription _constraintData
  ]


constraintDataSeries :: ConstraintData -> Series
constraintDataSeries _constraintData =
       "name"        .= constraintName _constraintData 
   <> "description" .= constraintDescription _constraintData
   


-- Constraint > Data > Name
--------------------------------------------------------------------------------

instance FromJSON ConstraintName where
  parseJSON (Yaml.String s) = return $ ConstraintName s
  parseJSON invalid         = typeMismatch "ConstraintName" invalid


instance ToJSON ConstraintName where
  toJSON     (ConstraintName name) = toJSON name
  toEncoding (ConstraintName name) = toEncoding name


-- Constraint > Data > Description
--------------------------------------------------------------------------------

instance FromJSON ConstraintDescription where
  parseJSON (Yaml.String s) = return $ ConstraintDescription s
  parseJSON invalid         = typeMismatch "ConstraintDescription" invalid


instance ToJSON ConstraintDescription where
  toJSON     (ConstraintDescription desc) = toJSON desc
  toEncoding (ConstraintDescription desc) = toEncoding desc


-- Constraint > Constraint'
--------------------------------------------------------------------------------

instance FromJSON Constraint' where
  parseJSON (Object m) = do 
    typeString <- m .: "type" :: Parser Text
    paramtersValue <- m .: "parameters" :: Parser Yaml.Value
    let failMessage = "Invalid constraint name. Please check the" ++
                      " documentation for a list of valid constraints."
    case typeString of
      "string_one_of"       -> StringOneOf <$> parseJSON paramtersValue
      "number_greater_than" -> NumGreaterThan <$> parseJSON paramtersValue
      _                     -> fail failMessage
  parseJSON invalid         = typeMismatch "Constraint'" invalid 


instance ToJSON Constraint' where
  toJSON (StringOneOf    _constraint) = toJSON _constraint
  toJSON (NumGreaterThan _constraint) = toJSON _constraint

  toEncoding (StringOneOf    _constraint) = toEncoding _constraint
  toEncoding (NumGreaterThan _constraint) = toEncoding _constraint


constraintSeries :: Constraint' -> Series
constraintSeries _constraint =
    ( "type"       .= (toJSON $ constraintTypeString _constraint)
   <> "parameters" .= toJSON _constraint
    )


constraintPairs :: Constraint' -> [Pair]
constraintPairs _constraint = 
  [ "type"       .= (toJSON $ constraintTypeString _constraint)
  , "parameters" .= toJSON _constraint
  ]

 
-- Constraint > String One Of
--------------------------------------------------------------------------------

instance FromJSON StringOneOfConstraint where
  parseJSON (Object m) = StringOneOfConstraint <$> m .: "set" 
  parseJSON invalid    = typeMismatch "StringOneOfConstraint" invalid 


instance ToJSON StringOneOfConstraint where
  toJSON stringOneOf = object [ "set" .= stringOneOfSet stringOneOf ]
  toEncoding stringOneOf = pairs ( "set" .= stringOneOfSet stringOneOf )


-- Constraint > Number Greater Than
--------------------------------------------------------------------------------

instance FromJSON NumberGreaterThanConstraint where
  parseJSON (Object m) = NumberGreaterThanConstraint
                              <$> m .: "greater_than" 
  parseJSON invalid    = typeMismatch "NumberGreaterThanConstraint" invalid 


instance ToJSON NumberGreaterThanConstraint where
  toJSON numGreaterThan = 
    object [ "greater_than" .= numberGreaterThanLowerBound numGreaterThan ]
  toEncoding numGreaterThan = 
    pairs ( "greater_than" .= numberGreaterThanLowerBound numGreaterThan )

