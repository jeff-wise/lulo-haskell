
{-| HTML Types

-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Lulo.HTML.Types where


import Control.Lens.TH (makeFields)



newtype HtmlSettings = HtmlSettings
  { _htmlSettingsCssFilePath :: Maybe FilePath
  }



makeFields ''HtmlSettings
