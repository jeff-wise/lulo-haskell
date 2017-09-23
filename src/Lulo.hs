
{-| Lulo Library Main Module
   
-}


module Lulo 
  ( module Lulo.Schema.Types
  , module Lulo.HTML.Types
  , module Lulo.Schema
  , schemaDivHtml
  ) where


import Lulo.Schema
import Lulo.Schema.Types
import Lulo.Schema.Index (schemaIndex)
import Lulo.HTML.Types
import Lulo.HTML (schemaDiv)

import Text.Blaze.Html5 (Html)



schemaDivHtml :: Schema -> HtmlSettings -> Html
schemaDivHtml schema = schemaDiv (schemaIndex schema)
