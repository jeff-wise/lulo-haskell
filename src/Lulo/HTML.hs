
{-| General HTML Types/Combinators

    Generate HTML documents from a Lulo specification.
-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML (
    specDoc
  , specDiv
  ) where


import Lulo.HTML.Types
import qualified Lulo.HTML.Spec.Content as Content (html)
import qualified Lulo.HTML.Spec.Sidebar as Sidebar (html)
import Lulo.Spec.Index (SpecIndex)

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
specDoc :: SpecIndex -> HtmlSettings -> Html
specDoc specIndex settings =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Specification"
      let mCSSFilePath = settingsCssFilePath settings
      case mCSSFilePath of
        Just path -> H.link ! A.rel "stylesheet" 
                                   ! A.type_ "text/css" 
                                   ! A.href (toValue path)
        Nothing   -> return ()   
      H.link ! A.href "https://fonts.googleapis.com/css?family=Lato:300,400,700"
             ! A.rel "stylesheet"
    H.body $ specDiv specIndex settings



specDiv :: SpecIndex -> HtmlSettings -> Html
specDiv specIndex _ =
  H.div ! A.id "specification" $ do
    H.div ! A.id "sidebar" $ Sidebar.html specIndex
    H.div ! A.id "content" $ Content.html specIndex




