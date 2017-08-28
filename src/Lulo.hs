
{-| Lulo Library Main Module
   
-}


module Lulo 
  ( module Lulo.Spec.Types
  , module Lulo.HTML.Types
  , specDivHtml
  ) where


import Lulo.Spec.Types
import Lulo.Spec.Index (specIndex)
import Lulo.HTML.Types
import Lulo.HTML (specDiv)

import Text.Blaze.Html5 (Html)



specDivHtml :: Spec -> HtmlSettings -> Html
specDivHtml spec = specDiv (specIndex spec)
