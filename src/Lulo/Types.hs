
{-| Types
-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Lulo.Types where


import Control.Lens.TH (makeFields)



-- PARAMETERS
--------------------------------------------------------------------------------

data Parameters = Parameters 
  { _parametersSpecFilename   :: FilePath 
  , _parametersVerbosity      :: Verbosity
  , _parametersHtmlFilename   :: Maybe FilePath
  , _parametersCssFilename    :: Maybe FilePath
  , _parametersHtmlFilePretty :: Bool
  }




-- Parameters > Verbosity
--------------------------------------------------------------------------------

data Verbosity = 
    Normal 
  | Verbose
  deriving (Eq)


makeFields ''Parameters
