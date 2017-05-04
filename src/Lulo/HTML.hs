
{-| HTML Generation

  Generate HTML documents from a Lulo specification.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML (
  specHTML
  ) where


import Lulo.Types

import Control.Lens ((^.))
import Control.Monad (forM_, mapM_, when)

import Data.Char (toLower)
import Data.Foldable (null)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html5 (Html, (!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A



-- DOCUMENT
--------------------------------------------------------------------------------

specHTML :: Spec -> Maybe FilePath -> Html
specHTML spec mCSSFilePath =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Specification"
      case mCSSFilePath of
        Just cssFilePath -> H.link ! A.rel "stylesheet" 
                                   ! A.type_ "text/css" 
                                   ! (A.href $ toValue cssFilePath)
        Nothing          -> return ()   
    H.body $ do 
      H.div ! A.id "specification" $ do
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
  containerHTML $ forM_ (spec ^. types) $ 
                    tellTypeHTML spec
  where
    containerHTML :: Html -> Html
    containerHTML = H.div ! A.id "types"


-- > TYPE
--------------------------------------------------------------------------------

tellTypeHTML :: Spec -> ObjectType -> Html
tellTypeHTML spec objectType = do
  -- Header
  H.h2 (toHtml $ objectType ^. common.label)
  -- Description
  case (objectType ^. common.description) of
    Just desc -> H.p ! A.class_ "description" $ toHtml desc
    Nothing   -> return ()
  -- Fields / Cases
  case (objectType ^. typeData) of
    Product productType -> tellTypeFieldsHTML spec productType
    Sum     sumType     -> tellTypeCasesHTML sumType
   

tellTypeFieldsHTML :: Spec -> ProductType -> Html
tellTypeFieldsHTML spec productType = do
  H.div ! A.class_ "fields" $ do
    H.h3 "Fields"
    H.ul $ forM_ (productType ^. fields) (fieldListItemHTML spec)


fieldListItemHTML :: Spec -> Field -> Html
fieldListItemHTML spec = H.li . fieldHTML spec


-- > FIELD
--------------------------------------------------------------------------------

fieldHTML :: Spec -> Field -> Html
fieldHTML spec field = do
  -- Name
  fieldNameHTML $ field ^. name
  -- Presence
  fieldPresenceHTML $ field ^. presence
  -- Type
  fieldTypeHTML $ field ^. valueType
  -- Description
  fieldDescriptionHTML $ field ^. description
  -- Constraints
  fieldConstraintsHTML (field ^. constraints) spec
  -- Default Value
  fieldDefaultValueHTML $ field ^. defaultValue


fieldNameHTML :: FieldName -> Html
fieldNameHTML fieldName = 
  H.h4 ! A.class_ "field-name" $ 
    (toHtml $ unFieldName fieldName)


fieldPresenceHTML :: Presence -> Html
fieldPresenceHTML fieldPresence = 
  H.div ! A.class_ "field-presence" $ 
    (toHtml $ map toLower $ show fieldPresence)


fieldTypeHTML :: ValueType -> Html
fieldTypeHTML fieldType = 
  H.div ! A.class_ "field-type" $ 
    (toHtml $ map toLower $ show fieldType)


fieldDescriptionHTML :: Maybe FieldDescription -> Html
fieldDescriptionHTML (Just fieldDescription) = 
  H.div ! A.class_ "field-description" $ do
    H.p $ toHtml $ unFieldDesc fieldDescription
fieldDescriptionHTML Nothing                 = return ()


fieldDefaultValueHTML :: Maybe FieldDefaultValue -> Html
fieldDefaultValueHTML (Just (FieldDefaultValue defaultValue)) =
  H.div ! A.class_ "field-default-value" $ do
    H.h4 "Default Value"
    H.span ! A.class_ "default-value" $ toHtml defaultValue
fieldDefaultValueHTML Nothing                                 = return ()


-- > CONSTRAINT
--------------------------------------------------------------------------------

fieldConstraintsHTML :: [ConstraintName] -> Spec -> Html
fieldConstraintsHTML constraintNames spec = do
  let constraints = catMaybes $ fmap (specConstraint spec) constraintNames 
  when (not $ null constraints) $
    H.div ! A.class_ "constraints" $ do
      H.h4 "Constraints"
      H.ul $ mapM_ fieldConstraintListItemHTML constraints


fieldConstraintListItemHTML :: ValueConstraint -> Html
fieldConstraintListItemHTML = H.li . fieldConstraintHTML


fieldConstraintHTML :: ValueConstraint -> Html
fieldConstraintHTML valueConstraint =
  case (valueConstraint ^. constraint) of
    StringOneOf    c -> stringOneOfConstraintHTML c
    NumGreaterThan c -> numGreaterThanConstraintHTML c


stringOneOfConstraintHTML :: StringOneOfConstraint -> Html
stringOneOfConstraintHTML oneOf = do
  H.div ! A.class_ "constraint-string-one-of" $ do
    H.span "Must be one of"
    forM_ (oneOf ^. set) $ 
      (H.span ! A.class_ "choice") . toHtml


numGreaterThanConstraintHTML :: NumberGreaterThanConstraint -> Html
numGreaterThanConstraintHTML greaterThan = 
  H.div ! A.class_ "constraint-number-greater-than" $ do
    H.span "> "
    H.span $ toHtml (greaterThan ^. lowerBound)


-- > CASE
--------------------------------------------------------------------------------

tellTypeCasesHTML :: SumType -> Html
tellTypeCasesHTML sumType = do
  H.div ! A.class_ "cases" $ do
    H.h3 "Cases"
    H.ul $ forM_ (sumType ^. cases) caseListItemHTML


caseListItemHTML :: SumCase -> Html
caseListItemHTML = H.li . caseHTML


caseHTML :: SumCase -> Html
caseHTML sumCase = do
  -- Name
  H.h4 ! A.class_ "case-name" $ 
    (toHtml $ unSumCaseName $ sumCase ^. name)
  -- Type
  H.div ! A.class_ "case-type" $ 
    (toHtml $ map toLower $ show $  sumCase ^. valueType)
  -- Description
  case (sumCase ^. description) of
    Just caseDescription -> 
      H.div ! A.class_ "case-description" $ do
        H.p $ toHtml $ unSumCaseDesc caseDescription
    Nothing              -> return ()


-- SHOW
--------------------------------------------------------------------------------

showHTML :: Spec -> Html
showHTML spec = showDivHTML $ showContentHTML spec
  where
    showDivHTML :: Html -> Html
    showDivHTML = H.div ! A.id "show"


showContentHTML :: Spec -> Html
showContentHTML spec = H.h1 "Show"


