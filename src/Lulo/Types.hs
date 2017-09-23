
{-| Types
-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Lulo.Types where




-- PARAMETERS
--------------------------------------------------------------------------------

data Parameters = Parameters 
  { parametersSchemaFilename      :: FilePath 
  , parametersVerbosity           :: Verbosity
  , parametersHtmlFilePath        :: Maybe FilePath
  , parametersHtmlOptionsFilePath :: Maybe FilePath
  -- , _parametersCssFilename    :: Maybe FilePath
  -- , _parametersHtmlFilePretty :: Bool
  }




-- Parameters > Verbosity
--------------------------------------------------------------------------------

data Verbosity = 
    Normal 
  | Verbose
  deriving (Eq)

