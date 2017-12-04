
{-| The Schema Schema

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Schema.SchemaSchema where


import Lulo.Schema.Types


import Data.Monoid ((<>))
import Data.Text (Text)


--------------------------------------------------------------------------------
-- SCHEMA
--------------------------------------------------------------------------------

schemaSchema :: Schema
schemaSchema = Schema 
  { schemaVersion      = SchemaVersion "1.0"
  , schemaMetadata     = SchemaMetadata (SchemaName "Schema") []
  , schemaDescription  = Just $ SchemaDescription schemaDescriptionText
  , schemaRootTypeName = CustomTypeName "schema"
  , schemaTypes        = types
  , schemaConstraints  = constraints
  }


schemaDescriptionText :: Text
schemaDescriptionText = 
     "Lulo is a data specification format. It allows you define schemas. With " 
  <> "schemas, you can define the structure of your data with a collection of "
  <> "Algebraic Data Types. Additionally, schemas may contain constraints, " 
  <> "sometimes called Refinement Types, on those types in order to precisely "
  <> "enforce the possible values of the data.\n\n"
  <> "Then you can:\n\n"
  <> " * Validate that a JSON / YAML document matches a schema.\n"
  <> " * Automatically parse a JSON / YAML document that matches a schema." 
  <> "Parsing is flexible -- you can map the parsed data to any data structure.\n"
  <> " * Generate HTML documentation for a schema.\n\n"
  <> "With Lulo you can declare your data types in schema files and use the same "
  <> "data in different parts of your application. The data will always be the "
  <> "same because the parsing is done automatically according to the types "
  <> "defined in the schemas.\n\n"
  <> "If your application provides an open data format for interfacing with an "
  <> "API or creating custom scripts, then clients will be able to create their "
  <> "own programs and use your data by just using Lulo and the data schemas. "
  <> "Plus, clients can use the generated HTML documentation as a guide.\n\n"
  <> "> Lulo is Defined With Lulo\n>\n"
  <> "> Lulo can be defined using Lulo, and in fact, it is. Schemas are just "
  <> "documents, and Lulo parses them the same way that it parse any other "
  <> "document. This implies the existence of a schema schema which defines "
  <> "the format of all schemas.\n>\n"
  <> "> The schema schema is hard-coded into the implementation because the "
  <> "schema schema is a schema, and we cannot parse a schema without the schema schema.\n\n"
  <> "Since Lulo itself is a schema, it's HTML documentation can be automatically "
  <> "generated -- it is self-documenting."


--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

types :: [CustomType]
types = [
  CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "schema"
    , prodTypeLabel       = CustomTypeLabel "The schema."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Schema"
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
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "constraint"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ]
    , prodTypeCodeExamples  = []
    , prodTypeOrder         = 0
    }
  , CustomTypePrim PrimCustomType
    { primTypeName         = CustomTypeName "schema_version"
    , primTypeLabel        = CustomTypeLabel "Version"
    , primTypeDescription  = Nothing
    , primTypeGroup        = Just $ CustomTypeGroup "Schema"
    , primTypeBaseType     = BaseTypePrim String
    , primTypeConstraints  = []
    , primTypeCodeExamples = []
    , primTypeOrder        = 5
    }
  , CustomTypePrim PrimCustomType
    { primTypeName         = CustomTypeName "schema_name"
    , primTypeLabel        = CustomTypeLabel "Name"
    , primTypeDescription  = Nothing
    , primTypeGroup        = Just $ CustomTypeGroup "Metadata"
    , primTypeBaseType     = BaseTypePrim String
    , primTypeConstraints  = []
    , primTypeCodeExamples = []
    , primTypeOrder        = 10
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "schema_author"
    , prodTypeLabel       = CustomTypeLabel "Author"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Metadata"
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
    , prodTypeCodeExamples  = []
    , prodTypeOrder       = 15
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "schema_metadata"
    , prodTypeLabel       = CustomTypeLabel "Metadata"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Metadata"
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
    , prodTypeCodeExamples  = []
    , prodTypeOrder         = 20
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "schema_description"
    , prodTypeLabel       = CustomTypeLabel "Description"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Schema"
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
    , prodTypeCodeExamples  = []
    , prodTypeOrder         = 25
    }
  , CustomTypeSum SumCustomType 
    { sumTypeName        = CustomTypeName "schema_type"
    , sumTypeLabel       = CustomTypeLabel "Type"
    , sumTypeDescription = Nothing
    , sumTypeGroup       = Just $ CustomTypeGroup "Type"
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
    , sumTypeCodeExamples  = []
    , sumTypeOrder       = 30
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "product_type"
    , prodTypeLabel       = CustomTypeLabel "Product Type"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Type"
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
      , Field
        { fieldName         = FieldName "code_examples"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "code_example"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    , prodTypeCodeExamples  = []
    , prodTypeOrder         = 35
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "sum_type"
    , prodTypeLabel       = CustomTypeLabel "Sum Type"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Type"
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
      , Field
        { fieldName         = FieldName "code_examples"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "code_example"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    , prodTypeCodeExamples  = []
    , prodTypeOrder         = 40
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "primitive_type"
    , prodTypeLabel       = CustomTypeLabel "Wrapper Type"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Type"
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
      , Field
        { fieldName         = FieldName "code_examples"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "code_example"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    , prodTypeCodeExamples  = []
    , prodTypeOrder         = 45
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "symbol_type"
    , prodTypeLabel       = CustomTypeLabel "Symbol Type"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Type"
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
      , Field
        { fieldName         = FieldName "code_examples"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "code_example"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    , prodTypeCodeExamples  = []
    , prodTypeOrder         = 50
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "type_name"
    , primTypeLabel       = CustomTypeLabel "Type Name"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Type"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 55
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "type_label"
    , primTypeLabel       = CustomTypeLabel "Type Label"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Type"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 60
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "type_description"
    , primTypeLabel       = CustomTypeLabel "Type Description"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Type"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 65
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "type_group"
    , primTypeLabel       = CustomTypeLabel "Type Group"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Type"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 70
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "code_example"
    , prodTypeLabel       = CustomTypeLabel "Code Example"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Code Example"
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "language"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "code_example_language"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "case"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "code_example_case"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "code"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "code_example_code"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "title"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "code_example_title"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }

      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "code_example_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    , prodTypeCodeExamples = []
    , prodTypeOrder         = 67
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "code_example_language"
    , primTypeLabel       = CustomTypeLabel "Code Example Language"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Code Example"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 70
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "code_example_case"
    , primTypeLabel       = CustomTypeLabel "Code Example Case"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Code Example"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 70
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "code_example_code"
    , primTypeLabel       = CustomTypeLabel "Code Example Code"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Code Example"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 70
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "code_example_title"
    , primTypeLabel       = CustomTypeLabel "Code Example Title"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Code Example"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 70
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "code_example_description"
    , primTypeLabel       = CustomTypeLabel "Code Example Description"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Code Example"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 70
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "field"
    , prodTypeLabel       = CustomTypeLabel "Field"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Field"
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
    , prodTypeCodeExamples = []
    , prodTypeOrder         = 70
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "field_name"
    , primTypeLabel       = CustomTypeLabel "Field Name"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Field"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 75
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "field_presence"
    , primTypeLabel       = CustomTypeLabel "Field Presence"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Field"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 80
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "field_description"
    , primTypeLabel       = CustomTypeLabel "Field Description"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Field"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 85
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "field_default_value"
    , primTypeLabel       = CustomTypeLabel "Field Default Value"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Field"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 90
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "case"
    , prodTypeLabel       = CustomTypeLabel "Case"
    , prodTypeDescription = Just $ CustomTypeDescription $ 
           "A case of a sum type. \n\n"
        <> "A sum type is a tagged union of types. Each type in the tagged union is a case.\n\n"
        <> "For example, suppose we have a Pet datatype represented with Haskell syntax:\n"
        <> "```\n"
        <> "data Pet = \n"
        <> "    PetDog Dog\n"
        <> "  | PetCat Cat\n"
        <> "  | PetHamster Hamster\n"
        <> "```\n\n"
        <> "Each data constructor of `Pet` (e.g. `PetDog`, `PetCat`, ...) is a case. The only difference is that Lulo doesn't have tags. "
        <> "Each case is uniquely identified by the type (e.g. `Dog`, `Cat`, ...), so every case in a sum type must "
        <> "represent a unique type."
    , prodTypeGroup       = Just $ CustomTypeGroup "Case"
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
    , prodTypeCodeExamples = [
        CodeExample
        { codeExampleLanguage    = "YAML"
        , codeExampleCase        = Nothing 
        , codeExampleCode        = 
               "type: sum_type\n"
            <> "sum_type:\n"
            <> "   name: question\n"
            <> "   label: Question\n"
            <> "   description: The types of questions on a quiz.\n"
            <> "   cases:\n"
            <> "   - type: multiple_choice\n"
            <> "     description: A question with a set of possible answers.\n"
            <> "   - type: open_ended\n"
            <> "     description: A question with a written answer.\n"
            <> "   - type: matching\n"
            <> "     description: > \n"
            <> "       A question where the user must match items on the left side \n"
            <> "       to items on the left side to the right side."
        , codeExampleTitle        = "A Question" 
        , codeExampleDescription  = Just "A sum type that represents a question with three cases."
        }
      ]
    , prodTypeOrder         = 95
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "case_description"
    , primTypeLabel       = CustomTypeLabel "Case Description"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Case"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 100
    }
  , CustomTypeSum SumCustomType 
    { sumTypeName        = CustomTypeName "base_type"
    , sumTypeLabel       = CustomTypeLabel "Wrappable Type"
    , sumTypeDescription = Nothing
    , sumTypeGroup       = Just $ CustomTypeGroup "Wrappable Type"
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
    , sumTypeCodeExamples = []
    , sumTypeOrder       = 105
    }
  , CustomTypeSum SumCustomType 
    { sumTypeName        = CustomTypeName "value_type"
    , sumTypeLabel       = CustomTypeLabel "Kind"
    , sumTypeDescription = Nothing
    , sumTypeGroup       = Just $ CustomTypeGroup "Kind"
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
    , sumTypeCodeExamples = []
    , sumTypeOrder       = 110
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "prim_type"
    , primTypeLabel       = CustomTypeLabel "Primitive Type"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Kind"
    , primTypeBaseType    = BaseTypeCustom $ CustomTypeName "prim_value_type"
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 115
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "prim_coll_type"
    , primTypeLabel       = CustomTypeLabel "Primitive Collection Type"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Kind"
    , primTypeBaseType    = BaseTypeCustom $ CustomTypeName "prim_value_type"
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 120
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "custom_type"
    , primTypeLabel       = CustomTypeLabel "Custom Type"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Kind"
    , primTypeBaseType    = BaseTypeCustom $ CustomTypeName "type_name"
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 125
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "custom_coll_type"
    , primTypeLabel       = CustomTypeLabel "Custom Collection Type"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Kind"
    , primTypeBaseType    = BaseTypeCustom $ CustomTypeName "type_name"
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 130
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "prim_value_type"
    , primTypeLabel       = CustomTypeLabel "Primivite Value Type"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Kind"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 135
    }
  , CustomTypeSum SumCustomType 
    { sumTypeName        = CustomTypeName "constraint"
    , sumTypeLabel       = CustomTypeLabel "Constraint"
    , sumTypeDescription = Nothing
    , sumTypeGroup       = Just $ CustomTypeGroup "Constraint"
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
    , sumTypeCodeExamples = []
    , sumTypeOrder       = 140
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "constraint_name"
    , primTypeLabel       = CustomTypeLabel "Constraint Name"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Constraint"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 145
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "constraint_label"
    , primTypeLabel       = CustomTypeLabel "Constraint Label"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Constraint"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 150
    }
  , CustomTypePrim PrimCustomType
    { primTypeName        = CustomTypeName "constraint_description"
    , primTypeLabel       = CustomTypeLabel "Constraint Description"
    , primTypeDescription = Nothing
    , primTypeGroup       = Just $ CustomTypeGroup "Constraint"
    , primTypeBaseType    = BaseTypePrim String
    , primTypeConstraints = []
    , primTypeCodeExamples = []
    , primTypeOrder       = 155
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "constraint_string_one_of"
    , prodTypeLabel       = CustomTypeLabel "String Membership Constraint"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Constraint"
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
        { fieldName         = FieldName "label"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "constraint_label"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "constraint_description"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "set"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = CustomList $ CustomTypeName "constraint_string_one_of_value"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    , prodTypeCodeExamples = []
    , prodTypeOrder         = 160
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "constraint_string_one_of_value"
    , prodTypeLabel       = CustomTypeLabel "String Membership Constraint Value."
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Constraint"
    , prodTypeFields      = [ 
        Field
        { fieldName         = FieldName "value"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Prim String
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Optional
        , fieldDescription  = Nothing
        , fieldValueType    = Prim String
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      ] 
    , prodTypeCodeExamples = []
    , prodTypeOrder         = 165
    }
  , CustomTypeProduct ProductCustomType 
    { prodTypeName        = CustomTypeName "constraint_num_greater_than"
    , prodTypeLabel       = CustomTypeLabel "Number Greater Than Constraint"
    , prodTypeDescription = Nothing
    , prodTypeGroup       = Just $ CustomTypeGroup "Constraint"
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
        { fieldName         = FieldName "label"
        , fieldPresence     = Required
        , fieldDescription  = Nothing
        , fieldValueType    = Custom $ CustomTypeName "constraint_label"
        , fieldConstraints  = []
        , fieldDefaultValue = Nothing
        }
      , Field
        { fieldName         = FieldName "description"
        , fieldPresence     = Required
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
    , prodTypeCodeExamples = []
    , prodTypeOrder         = 170
    }
  ]


--------------------------------------------------------------------------------
-- CONSTRAINTS
--------------------------------------------------------------------------------

constraints :: [Constraint]
constraints = []
