
{-| The Schema Schema

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Schema.SchemaSchema where


import Lulo.Schema.Types



--------------------------------------------------------------------------------
-- SCHEMA
--------------------------------------------------------------------------------

schemaSchema :: Schema
schemaSchema = Schema 
  { schemaVersion      = SchemaVersion "1.0"
  , schemaMetadata     = SchemaMetadata (SchemaName "schema-schema") []
  , schemaDescription  = Nothing
  , schemaRootTypeName = Just $ CustomTypeName "schema"
  , schemaTypes        = types
  , schemaConstraints  = constraints
  }


--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

types :: [CustomType]
types = [
  CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "schema"
    , prodTypeLabel       = CustomTypeLabel "The schema."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "version"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "schema_version"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "metadata"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "schema_metadata"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "schema_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "root_type"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "types"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "schema_type"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "constraints"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "constraint"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ]
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "schema_version"
    , primTypeLabel       = CustomTypeLabel "The schema version."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "schema_name"
    , primTypeLabel       = CustomTypeLabel "The schema name."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "schema_author"
    , prodTypeLabel       = CustomTypeLabel "The schema author."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
      Field
      { fieldName         = FieldName "name"
      , fieldPresence     = Required
      , fieldDescription  = Nothing
      , fieldValueType    = Prim String
      , fieldConstraints  = []
      , fieldDefaultValue = Nothing
      }
      ] 
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "schema_metadata"
    , prodTypeLabel       = CustomTypeLabel "The schema metadata."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "name"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "schema_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "authors"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "schema_author"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "schema_description"
    , prodTypeLabel       = CustomTypeLabel "The schema description."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "overview"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Prim String
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    }
  , CustomTypeSum SumCustomType 
    { sumTypeName        = CustomTypeName "schema_type"
    , sumTypeLabel       = CustomTypeLabel "A schema type."
    , sumTypeDescription = Nothing
    , sumTypeGroup       = Nothing
    , sumTypeCases       = [ 
        Case
        { caseType        = CustomTypeName "product_type"
        , caseDescription = Nothing
        }
      , Case
        { caseType        = CustomTypeName "sum_type"
        , caseDescription = Nothing
        }
      , Case
        { caseType        = CustomTypeName "primitive_type"
        , caseDescription = Nothing
        }
      , Case
        { caseType        = CustomTypeName "symbol_type"
        , caseDescription = Nothing
        }
      ] 
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "product_type"
    , prodTypeLabel       = CustomTypeLabel "A product type."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "name"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "label"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_label"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "group"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_group"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "constraints"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "constraint_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "fields"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "field"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "sum_type"
    , prodTypeLabel       = CustomTypeLabel "A sum type."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "name"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "label"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_label"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "group"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_group"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "constraints"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "constraint_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "cases"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "case"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "primitive_type"
    , prodTypeLabel       = CustomTypeLabel "A wrapper type."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "name"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "label"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_label"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "group"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_group"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "constraints"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "constraint_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "base_type"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "base_type"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "symbol_type"
    , prodTypeLabel       = CustomTypeLabel "A symbol type."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "name"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "label"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_label"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "group"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_group"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "constraints"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "constraint_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "symbol"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Prim String
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "type_name"
    , primTypeLabel       = CustomTypeLabel "A type name."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "type_label"
    , primTypeLabel       = CustomTypeLabel "A type label."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "type_description"
    , primTypeLabel       = CustomTypeLabel "A type description."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "type_group"
    , primTypeLabel       = CustomTypeLabel "A type group."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "field"
    , prodTypeLabel       = CustomTypeLabel "A field."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "name"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "field_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "presence"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "field_presence"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "field_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "type"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "value_type"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "default_value"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "field_default_value"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "field_name"
    , primTypeLabel       = CustomTypeLabel "A field name."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "field_presence"
    , primTypeLabel       = CustomTypeLabel "optional / required."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "field_description"
    , primTypeLabel       = CustomTypeLabel "A field description."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "field_default_value"
    , primTypeLabel       = CustomTypeLabel "A field default value."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "case"
    , prodTypeLabel       = CustomTypeLabel "A case."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "type"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "type_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "case_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "case_description"
    , primTypeLabel       = CustomTypeLabel "A case description."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypeSum SumCustomType 
    { sumTypeName        = CustomTypeName "base_type"
    , sumTypeLabel       = CustomTypeLabel "The type of a value."
    , sumTypeDescription = Nothing
    , sumTypeGroup       = Nothing
    , sumTypeCases       = [ 
        Case
        { caseType        = CustomTypeName "prim_type"
        , caseDescription = Nothing
        }
      , Case
        { caseType        = CustomTypeName "custom_type"
        , caseDescription = Nothing
        }
      ] 
    }
  , CustomTypeSum SumCustomType 
    { sumTypeName        = CustomTypeName "value_type"
    , sumTypeLabel       = CustomTypeLabel "The type of a value."
    , sumTypeDescription = Nothing
    , sumTypeGroup       = Nothing
    , sumTypeCases       = [ 
        Case
        { caseType        = CustomTypeName "prim_type"
        , caseDescription = Nothing
        }
      , Case
        { caseType        = CustomTypeName "prim_coll_type"
        , caseDescription = Nothing
        }
      , Case
        { caseType        = CustomTypeName "custom_type"
        , caseDescription = Nothing
        }
      , Case
        { caseType        = CustomTypeName "custom_coll_type"
        , caseDescription = Nothing
        }
      ] 
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "prim_type"
    , primTypeLabel       = CustomTypeLabel "A primitive type."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypeCustom $ CustomTypeName "prim_value_type"
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "prim_coll_type"
    , primTypeLabel       = CustomTypeLabel "A primitive collection type."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypeCustom $ CustomTypeName "prim_value_type"
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "custom_type"
    , primTypeLabel       = CustomTypeLabel "A custom type."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypeCustom $ CustomTypeName "type_name"
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "custom_coll_type"
    , primTypeLabel       = CustomTypeLabel "A custom collection type."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypeCustom $ CustomTypeName "type_name"
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "prim_value_type"
    , primTypeLabel       = CustomTypeLabel "A primitive value type."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypeSum SumCustomType 
    { sumTypeName        = CustomTypeName "constraint"
    , sumTypeLabel       = CustomTypeLabel "A constraint."
    , sumTypeDescription = Nothing
    , sumTypeGroup       = Nothing
    , sumTypeCases       = [ 
        Case
        { caseType        = CustomTypeName "constraint_string_one_of"
        , caseDescription = Nothing
        }
      , Case
        { caseType        = CustomTypeName "constraint_num_greater_than"
        , caseDescription = Nothing
        }
      ] 
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "constraint_name"
    , primTypeLabel       = CustomTypeLabel "A constraint name."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "constraint_description"
    , primTypeLabel       = CustomTypeLabel "A constraint description."
    , primTypeDescription = Nothing
    , primTypeGroup       = Nothing
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "constraint_string_one_of"
    , prodTypeLabel       = CustomTypeLabel "String one of constraint."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "name"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "constraint_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "constraint_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "set"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = PrimList String
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "constraint_num_greater_than"
    , prodTypeLabel       = CustomTypeLabel "Number greater than constraint."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Nothing
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "name"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "constraint_name"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "constraint_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "lower_bound"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Prim Number
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    }



  ]


--------------------------------------------------------------------------------
-- CONSTRAINTS
--------------------------------------------------------------------------------

constraints :: [Constraint]
constraints = []