
{-| HTML Types

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML.Types where


import Data.Aeson



data HtmlSettings = HtmlSettings
  { htmlSettingsCssFilePath :: FilePath
  , htmlSettingsPrintPretty :: Bool
  }


defaultHtmlSettings :: HtmlSettings
defaultHtmlSettings = HtmlSettings 
  { htmlSettingsCssFilePath = "style.css"
  , htmlSettingsPrintPretty = False
  }



instance FromJSON HtmlSettings where
  parseJSON = withObject "HtmlSettings" $ \v -> HtmlSettings
      <$> v .:? "css_filepath" .!= "style.css"
      <*> v .:? "print_pretty" .!= False

