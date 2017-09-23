
{-| General HTML Types/Combinators

    Generate HTML documents from a Lulo specification.
-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML (
    schemaDoc
  , schemaDiv
  ) where


import Lulo.HTML.Types
import qualified Lulo.HTML.Spec.Content as Content (html)
import qualified Lulo.HTML.Spec.Sidebar as Sidebar (html)
import Lulo.Schema.Index (SchemaIndex)

import Data.Monoid ((<>))

import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html5 (
    Html
  , toValue
  , (!)
  )
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H



--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

-- | Create a complete HTML document containing the spec information
schemaDoc :: SchemaIndex -> HtmlSettings -> Html
schemaDoc schemaIndex settings =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Schema"
      H.link ! A.rel "stylesheet" 
             ! A.type_ "text/css" 
             ! A.href (toValue $ htmlSettingsCssFilePath settings)
      H.link ! A.rel "stylesheet"
             ! A.href "https://fonts.googleapis.com/css?family=Lato:400,700"
    H.body $ do
      schemaDiv schemaIndex settings
      H.script $ preEscapedToHtml scriptText


schemaDiv :: SchemaIndex -> HtmlSettings -> Html
schemaDiv schemaIndex _ =
  H.div ! A.id "schema" $ do
    H.div ! A.id "sidebar" $ Sidebar.html schemaIndex
    H.div ! A.id "content" $ Content.html schemaIndex



scriptText :: String
scriptText = 
     "document.addEventListener('DOMContentLoaded', function(event) { "
  <> "    document.querySelector('.section-header.types').addEventListener('click', function(e) {"
  <> "         var indexTypesElement = document.querySelector('#index-types');   " 
  <> "         if (indexTypesElement.classList.contains('open')) {" 
  <> "             indexTypesElement.classList.remove('open');" 
  <> "             indexTypesElement.classList.add('closed');" 
  <> "             document.querySelector('.section-header.types .caret').innerHTML = '&#x25B6';" 
  <> "         } else {" 
  <> "             indexTypesElement.classList.remove('closed');" 
  <> "             indexTypesElement.classList.add('open');" 
  <> "             document.querySelector('.section-header.types .caret').innerHTML = '&#x25BC';" 
  <> "         }" 
  <> "    });"              
  <> "    document.querySelector('.section-header.introduction').addEventListener('click', function(e) {"
  <> "         var indexTypesElement = document.querySelector('#introduction-sections');   " 
  <> "         if (indexTypesElement.classList.contains('open')) {" 
  <> "             indexTypesElement.classList.remove('open');" 
  <> "             indexTypesElement.classList.add('closed');" 
  <> "             document.querySelector('.section-header.introduction .caret').innerHTML = '&#x25B6';" 
  <> "         } else {" 
  <> "             indexTypesElement.classList.remove('closed');" 
  <> "             indexTypesElement.classList.add('open');" 
  <> "             document.querySelector('.section-header.introduction .caret').innerHTML = '&#x25BC';" 
  <> "         }" 
  <> "    });"              
  <> "    document.querySelector('.section-header.constraints').addEventListener('click', function(e) {"
  <> "         var indexTypesElement = document.querySelector('#index-constraints');   " 
  <> "         if (indexTypesElement.classList.contains('open')) {" 
  <> "             indexTypesElement.classList.remove('open');" 
  <> "             indexTypesElement.classList.add('closed');" 
  <> "             document.querySelector('.section-header.constraints .caret').innerHTML = '&#x25B6';" 
  <> "         } else {" 
  <> "             indexTypesElement.classList.remove('closed');" 
  <> "             indexTypesElement.classList.add('open');" 
  <> "             document.querySelector('.section-header.constraints .caret').innerHTML = '&#x25BC';" 
  <> "         }" 
  <> "    });"              
  <> "});"
