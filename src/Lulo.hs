
{-| Lulo Library Main Module
   
-}


module Lulo 
  ( module Lulo.Schema.Types
  , module Lulo.HTML.Types
  , specDivHtml
  ) where


import Lulo.Schema.Types
import Lulo.Schema.Index (schemaIndex)
import Lulo.HTML.Types
import Lulo.HTML (specDiv)

import Text.Blaze.Html5 (Html)



specDivHtml :: Schema -> HtmlSettings -> Html
specDivHtml schema = specDiv (schemaIndex schema)
