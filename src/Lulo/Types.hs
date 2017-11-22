
{-| Types
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Lulo.Types where


import Lulo.Schema (DocumentParseError)

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)

import Data.Monoid ((<>))



--------------------------------------------------------------------------------
-- COMMAND LINE INTERFACE
--------------------------------------------------------------------------------

-- CLI > Error
--------------------------------------------------------------------------------

newtype CLIError = 
  CouldNotParseSchema DocumentParseError


instance Show CLIError where
  show (CouldNotParseSchema err) = "Could Not Parse Schema:\n\n" <> show err


-- CLI > Monad
--------------------------------------------------------------------------------

newtype CLI a = CLI
  { getCLI ::  ExceptT CLIError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CLIError)


--------------------------------------------------------------------------------
-- CLI > PARAMETERS
--------------------------------------------------------------------------------

-- CLI > Parameters > Html
--------------------------------------------------------------------------------

data HtmlParameters = HtmlParameters 
  { htmlParamsSchemaFilename  :: FilePath 
  , htmlParamsVerbosity       :: Verbosity
  , htmlParamsHtmlFilePath    :: FilePath
  , htmlParamsOptionsFilePath :: Maybe FilePath
  } deriving (Eq, Show)


-- CLI > Parameters > Verbosity
--------------------------------------------------------------------------------

data Verbosity = 
    Normal 
  | Verbose
  deriving (Eq, Show)


-- CLI > Parameters > Schema Schema
--------------------------------------------------------------------------------

data SchemaSchemaParameters = SchemaSchemaParameters
  { ssParamsCommand             :: SchemaSchemaCommand
  , ssParamsHtmlOptionsFilePath :: Maybe FilePath
  } deriving (Eq, Show)


data SchemaSchemaCommand = 
    SchemaSchemaHtml
  deriving (Eq, Show)


instance Read SchemaSchemaCommand where
  readsPrec _ s = 
    case s of
      "html" -> [(SchemaSchemaHtml, "")]
      _      -> []
                      

-- CLI > Parameters > Check
--------------------------------------------------------------------------------

-- data CheckParameters = CheckParameters 
--   { parametersSchemaFilename      :: FilePath 
--   , parametersVerbosity           :: Verbosity
--   , parametersHtmlFilePath        :: Maybe FilePath
--   , parametersHtmlOptionsFilePath :: Maybe FilePath
--   }

