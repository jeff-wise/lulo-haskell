
{-| Sidebar

-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML.Spec.Sidebar where


import Lulo.HTML.Spec.Combinators (anchorLink)
import Lulo.Schema.Index (
    SchemaIndex
  , schemaIndexVersion
  , sortedConstraintsASC
  , typesByGroupAsc
  )
import Lulo.Schema.Types

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS (toList)
import Data.Foldable (forM_)
import Data.Monoid
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T (toLower)

import Text.Blaze.Html (preEscapedToHtml)
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
    H.span ! A.class_ "version-label" $ "version"
    H.span ! A.class_ "version" $ toHtml $ getSchemaVersion _version
    H.span ! A.class_ "caret" $ preEscapedToHtml ("&#9660;" :: Text)


--------------------------------------------------------------------------------
-- INDEX
--------------------------------------------------------------------------------

indexHtml :: SchemaIndex -> Html
indexHtml specIndex = do
  H.div ! A.class_ "section" $ do
    indexSectionHeaderHTML "Introduction"
    H.div ! A.id "introduction-sections" ! A.class_ "closed" $
      indexIntroductionHeaderHtml "Overview" "#introduction-overview"
  H.div ! A.class_ "section" $ do
    indexSectionHeaderHTML "Types"
    H.div ! A.id "index-types" ! A.class_ "closed" $
      forM_ (typesByGroupAsc specIndex) (uncurry indexTypesGroupHTML)
  H.div ! A.class_ "section" $ do
    indexSectionHeaderHTML "Constraints"
    H.div ! A.id "index-constraints" ! A.class_ "closed" $
      H.ul $ 
        forM_ (sortedConstraintsASC specIndex) indexConstraintLinkHtml


indexTypesGroupHTML :: CustomTypeGroup -> HashSet CustomType -> Html
indexTypesGroupHTML (CustomTypeGroup _group) customTypeSet =
  H.div ! A.class_ "group" $ do
    indexSectionSubHeaderHTML _group
    H.ul ! A.class_ (toValue _group) $
      indexTypeLinkListItemsHTML customTypeSet


indexConstraintLinkHtml :: Constraint -> Html
indexConstraintLinkHtml constraint = 
  H.li $ H.a $ H.h5 $ toHtml $ getConstraintLabel $ constraintLabel constraint


indexTypeLinkListItemsHTML :: HashSet CustomType -> Html
indexTypeLinkListItemsHTML customTypeSet = do 
  let sortedTypes = sortOn typeName $ HS.toList customTypeSet
  mapM_ typeLinkListItem sortedTypes
  where
    typeLinkListItem = H.li . typeLink
    typeLink _type =  indexLinkHtml
                        -- Link Display Text
                        (getCustomTypeLabel $ typeLabel _type)         
                        -- Link Href
                        (getCustomTypeName $ typeName _type) 


indexIntroductionHeaderHtml :: Text -> Text -> Html
indexIntroductionHeaderHtml headerText headerLink = 
  H.div ! A.class_ "subsection-header" $ 
    H.a ! A.href (toValue headerLink) $
      H.h3 $ toHtml headerText



-- Index > Components
--------------------------------------------------------------------------------

-- Index > Components > Link
--------------------------------------------------------------------------------

indexLinkHtml :: Text -> Text -> Html 
indexLinkHtml linkName linkHref =
  H.a ! A.class_ "object" 
      ! A.href (anchorLink linkHref)
      $ H.h5 $ toHtml linkName 


-- Index Components > Headers
--------------------------------------------------------------------------------

indexSectionHeaderHTML :: Text -> Html
indexSectionHeaderHTML headerText = do
  let classes = "section-header " <> T.toLower headerText
  H.div ! A.class_ (toValue classes) $ do
    H.div ! A.class_ "caret" $ preEscapedToHtml ("&#x25B6" :: Text)
    H.h2 $ toHtml headerText


indexSectionSubHeaderHTML :: Text -> Html
indexSectionSubHeaderHTML subheaderText = 
  H.div ! A.class_ "subsection-header" $ 
    H.a ! A.href (anchorLink subheaderText) $
      H.h3 $ toHtml subheaderText


