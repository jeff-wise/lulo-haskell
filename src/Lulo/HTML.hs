
{-| HTML Generation

  Generate HTML documents from a Lulo specification.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML (
  indexHTML
  ) where


import Lulo.Types

import Control.Lens
import Control.Monad (mapM_)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html5 (Html, (!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A



indexHTML :: Spec -> Html
indexHTML spec = indexDivHTML $ indexContentHTML spec


indexDivHTML :: Html -> Html
indexDivHTML = H.div ! A.id "index"


indexContentHTML :: Spec -> Html
indexContentHTML spec = do
  indexSectionHeaderHTML "Types"
  H.ul ! A.id "index_types" $ 
    indexTypeLinksHTML (spec ^. types)


indexTypeLinksHTML :: [ObjectType] -> Html
indexTypeLinksHTML = mapM_ typeLink
  where
    typeLink objectType = indexLinkHTML 
                            -- Link Display Text
                            (objectType ^. common.label)         
                            -- Link Href
                            ('#' `T.cons` (objectType ^. common.name))


indexLinkHTML :: Text -> Text -> Html 
indexLinkHTML linkName linkHref =
  H.a ! A.class_ "index_link" 
      ! (A.href $ toValue linkHref)
      $ toHtml linkName 


indexSectionHeaderHTML :: Text -> Html
indexSectionHeaderHTML headerText = H.h5 $ toHtml headerText

