
{-| Lulo Value

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Value where


import Lulo.Doc.Types

import Data.Monoid
import Data.Text (Text)



type ValueParser a = Either ValueParseError a



data ValueParseError = 
    ValueParseErrorUnexpectedType UnexpectedTypeError
  | ValueParseErrorMissingKey MissingKeyError
  | ValueParseErrorUnknownCase UnknownCaseError
  | ValueParseErrorUnexpectedValue UnexpectedValueError


instance Show ValueParseError where
  show (ValueParseErrorUnexpectedType err) = "ValueParseError:\n" <> show err
  show (ValueParseErrorMissingKey err) = "ValueParseError:\n" <> show err
  show (ValueParseErrorUnknownCase err) = "ValueParseError:\n" <> show err
  show (ValueParseErrorUnexpectedValue err) = "ValueParseError:\n" <> show err



data UnexpectedTypeError = UnexpectedTypeError
  { unexpectedTypeErrorExpectedType :: DocType
  , unexpectedTypeErrorActualType   :: DocType
  , unexpectedTypeErrorPath         :: DocPath
  }


instance Show UnexpectedTypeError where
  show (UnexpectedTypeError expected actual path) = 
    "Unexpected Type Error:\n" <> 
    "   Expected: " <> show expected <> "\n" <> 
    "   Actual: " <> show actual <> "\n" <> 
    "   Path: " <> show path


data MissingKeyError = MissingKeyError
  { missingKeyErrorKey  :: Text
  , missingKeyErrorPath :: DocPath
  }


instance Show MissingKeyError where
  show (MissingKeyError key path) = 
    "Missing Key:\n" <> 
    "   Key: " <> show key <> "\n" <> 
    "   Path: " <> show path


data UnknownCaseError = UnknownCaseError
  { unknownCaseErrorCase :: Text
  , unknownCaseErrorType :: Text
  , unknownCaseErrorPath :: DocPath
  }


instance Show UnknownCaseError where
  show (UnknownCaseError _case typeName path) = 
    "Unknown Case:\n" <> 
    "   Case: " <> show _case <> "\n" <> 
    "   Type: " <> show typeName <> "\n" <> 
    "   Path: " <> show path


data UnexpectedValueError = UnexpectedValueError
  { unexpectedValueErrorTypeName :: Text
  , unexpectedValueErrorValue    :: Text
  , unexpectedValueErrorPath     :: DocPath
  }


instance Show UnexpectedValueError where
  show (UnexpectedValueError typeName value path) = 
    "Unexpected Value:\n" <> 
    "   Type: " <> show typeName <> "\n" <> 
    "   Value: " <> show value <> "\n" <> 
    "   Path: " <> show path
