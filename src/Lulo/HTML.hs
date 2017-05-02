
{-| HTML Generation

  Generate HTML documents from a Lulo specification.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML (
  specHTML
  ) where


import Lulo.Types

import Control.Lens
import Control.Monad (forM_, mapM_)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html5 (Html, (!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A



-- DOCUMENT
--------------------------------------------------------------------------------

specHTML :: Spec -> Html
specHTML spec = do
  H.div ! A.id "index-container" $ indexHTML spec
  H.div ! A.id "tell-container" $ tellHTML spec
  H.div ! A.id "show-container" $ showHTML spec


-- INDEX
--------------------------------------------------------------------------------

indexHTML :: Spec -> Html
indexHTML spec = indexDivHTML $ indexContentHTML spec
  where
    indexDivHTML :: Html -> Html
    indexDivHTML = H.div ! A.id "index"

indexContentHTML :: Spec -> Html
indexContentHTML spec = 
  indexTypesSectionHTML (spec ^. types)

-- INDEX > SECTIONS
--------------------------------------------------------------------------------

-- ** Types Section
--------------------------------------------------------------------------------

indexTypesSectionHTML :: [ObjectType] -> Html
indexTypesSectionHTML types = do
  indexSectionHeaderHTML "Types"
  H.ul ! A.id "index-types" $ 
    indexTypeLinkListItemsHTML types

indexTypeLinkListItemsHTML :: [ObjectType] -> Html
indexTypeLinkListItemsHTML = mapM_ typeLinkListItem
  where
    typeLinkListItem = H.li . typeLink
    typeLink objectType = indexLinkHTML 
                            -- Link Display Text
                            (objectType ^. common.label)         
                            -- Link Href
                            ('#' `T.cons` (objectType ^. common.name))

-- INDEX > COMPONENTS
--------------------------------------------------------------------------------

-- ** Link
--------------------------------------------------------------------------------

indexLinkHTML :: Text -> Text -> Html 
indexLinkHTML linkName linkHref =
  H.a ! A.class_ "index-link" 
      ! (A.href $ toValue linkHref)
      $ toHtml linkName 

-- ** Header
--------------------------------------------------------------------------------

indexSectionHeaderHTML :: Text -> Html
indexSectionHeaderHTML headerText = H.h5 $ toHtml headerText


-- TELL
--------------------------------------------------------------------------------

tellHTML :: Spec -> Html
tellHTML spec = containerHTML $ tellContentHTML spec
  where
    containerHTML :: Html -> Html
    containerHTML = H.div ! A.id "tell"


tellContentHTML :: Spec -> Html
tellContentHTML spec = do
  tellTypesSectionHTML spec


tellTypesSectionHTML :: Spec -> Html
tellTypesSectionHTML spec = 
  containerHTML $ forM_ (spec ^. types) tellTypeHTML
  where
    containerHTML :: Html -> Html
    containerHTML = H.div ! A.id "types"


tellTypeHTML :: ObjectType -> Html
tellTypeHTML objectType = do
  -- Header
  H.h2 (toHtml $ objectType ^. common.label)
  -- Description
  case (objectType ^. common.description) of
    Just desc -> H.p ! A.class_ "description" $ toHtml desc
    Nothing   -> return ()


-- SHOW
--------------------------------------------------------------------------------

showHTML :: Spec -> Html
showHTML spec = showDivHTML $ showContentHTML spec
  where
    showDivHTML :: Html -> Html
    showDivHTML = H.div ! A.id "show"


showContentHTML :: Spec -> Html
showContentHTML spec = H.h1 "Show"


