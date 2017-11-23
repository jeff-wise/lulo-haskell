
{-| HTML Types

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML.Types where


import Data.Aeson



data HtmlSettings = HtmlSettings
  { htmlSettingsCssPaths     :: [FilePath]
  , htmlSettingsJsPaths      :: [FilePath]
  , htmlSettingsGoogleFonts  :: [String]
  , htmlSettingsPrintPretty  :: Bool
  }


defaultHtmlSettings :: HtmlSettings
defaultHtmlSettings = HtmlSettings 
  { htmlSettingsCssPaths    = ["schema.css"]
  , htmlSettingsJsPaths     = ["schema.js"]
  , htmlSettingsGoogleFonts = ["Lato:400,700","Source+Code+Pro"]
  , htmlSettingsPrintPretty = False
  }


instance FromJSON HtmlSettings where
  parseJSON = withObject "HtmlSettings" $ \v -> HtmlSettings
      <$> v .:? "css_paths"    .!= ["schema.css"]
      <*> v .:? "js_paths"     .!= ["schema.js"]
      <*> v .:? "google_fonts" .!= ["Lato:400,700","Source+Code+Pro"]
      <*> v .:? "print_pretty" .!= False

