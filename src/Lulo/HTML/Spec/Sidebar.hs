
{-| Sidebar

-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML.Spec.Sidebar where


import Lulo.HTML.Spec.Combinators (anchorLink)
import Lulo.Schema.Index (
    SchemaIndex
  , schemaIndexVersion
  , typesByGroupAsc
  )
import Lulo.Schema.Types

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



html :: SchemaIndex -> Html
html specIndex = 
  H.div ! A.id "index" $ do
    versionHTML $ schemaIndexVersion specIndex
    indexHtml specIndex
    

--------------------------------------------------------------------------------
-- VERSION
--------------------------------------------------------------------------------

versionHTML :: SchemaVersion -> Html
versionHTML _version =
  H.div ! A.id "version" $ do
    H.span ! A.class_ "version-label" $ "Version"
    H.span ! A.class_ "version" $ toHtml $ getSchemaVersion _version


--------------------------------------------------------------------------------
-- INDEX
--------------------------------------------------------------------------------

indexHtml :: SchemaIndex -> Html
indexHtml specIndex = do
  indexSectionHeaderHTML "Introduction"
  indexSectionHeaderHTML "Types"
  H.div ! A.id "index-types" $ 
    forM_ (typesByGroupAsc specIndex) (uncurry indexTypesGroupHTML)


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
                        (getCustomTypeLabel $ typeLabel _type)         
                        -- Link Href
                        (getCustomTypeName $ typeName _type) 


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


