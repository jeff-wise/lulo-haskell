
{-| Sidebar

-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML.Spec.Sidebar where


import Lulo.HTML.Spec.Combinators (anchorLink)
import Lulo.Spec.Index (
    SpecIndex
  , specVersion
  , specTypesByGroup
  )
import Lulo.Spec.Types

import Control.Lens

import Data.HashSet (HashSet)
import Data.Foldable (forM_)
import Data.Text (Text)

import Text.Blaze.Html5 (
    Html
  , toHtml, toValue
  , (!)
  )
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H



html :: SpecIndex -> Html
html specIndex = 
  H.div ! A.id "index" $ do
    versionHTML $ specVersion specIndex
    indexHtml specIndex
    

--------------------------------------------------------------------------------
-- VERSION
--------------------------------------------------------------------------------

versionHTML :: SpecVersion -> Html
versionHTML _version =
  H.div ! A.id "version" $ 
    toHtml $ "Version " ++ show _version


--------------------------------------------------------------------------------
-- INDEX
--------------------------------------------------------------------------------

indexHtml :: SpecIndex -> Html
indexHtml specIndex = do
  indexSectionHeaderHTML "Introduction"
  indexSectionHeaderHTML "Types"
  H.div ! A.id "index-types" $ 
    forM_ (specTypesByGroup specIndex) (uncurry indexTypesGroupHTML)


indexTypesGroupHTML :: CustomTypeGroup -> HashSet CustomType -> Html
indexTypesGroupHTML (CustomTypeGroup _group) customTypes = do
  indexSectionSubHeaderHTML _group
  H.ul ! A.class_ (toValue _group) $
    indexTypeLinkListItemsHTML customTypes


indexTypeLinkListItemsHTML :: HashSet CustomType -> Html
indexTypeLinkListItemsHTML = mapM_ typeLinkListItem
  where
    typeLinkListItem = H.li . typeLink
    typeLink _type =  indexLinkHtml
                        -- Link Display Text
                        (_type ^. typeData.label.text)         
                        -- Link Href
                        (_type ^. typeData.name.text) 


-- Index > Components
--------------------------------------------------------------------------------

-- Index > Components > Link
--------------------------------------------------------------------------------

indexLinkHtml :: Text -> Text -> Html 
indexLinkHtml linkName linkHref =
  H.a ! A.class_ "object" 
      ! A.href (anchorLink linkHref)
      $ toHtml linkName 


-- Index Components > Headers
--------------------------------------------------------------------------------

indexSectionHeaderHTML :: Text -> Html
indexSectionHeaderHTML headerText = 
  H.h2 $ 
    H.a ! A.href (anchorLink headerText)
        $ toHtml headerText


indexSectionSubHeaderHTML :: Text -> Html
indexSectionSubHeaderHTML subheaderText = 
  H.h3 $ 
    H.a ! A.href (anchorLink subheaderText)
        $ toHtml subheaderText


