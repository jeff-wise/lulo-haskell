
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
import qualified Data.HashMap.Lazy as HML (toList)
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
      H.link ! A.href "https://fonts.googleapis.com/css?family=Open+Sans"
             ! A.rel "stylesheet"
    H.body $ do 
      H.div ! A.id "specification" $ do
        H.div ! A.id "sidebar" $ indexHTML spec
        H.div ! A.id "content" $ contentHTML spec


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


-- Index > Sections
--------------------------------------------------------------------------------

-- Index > Sections > Types
--------------------------------------------------------------------------------

indexTypesSectionHTML :: [ObjectType] -> Html
indexTypesSectionHTML types = do
  indexSectionHeaderHTML "Types"
  let typesByGroupList = HML.toList $ groupToTypeMap types
  H.div ! A.id "index-types" $ 
    forM_ typesByGroupList (uncurry indexTypesGroupHTML)


indexTypesGroupHTML :: Text -> [ObjectType] -> Html
indexTypesGroupHTML groupName objectTypes = do
  indexSectionSubHeaderHTML groupName
  H.ul ! (A.class_ $ toValue groupName) $
    indexTypeLinkListItemsHTML objectTypes


indexTypeLinkListItemsHTML :: [ObjectType] -> Html
indexTypeLinkListItemsHTML = mapM_ typeLinkListItem
  where
    typeLinkListItem = H.li . typeLink
    typeLink objectType = indexLinkHTML 
                            -- Link Display Text
                            (objectType ^. common.label)         
                            -- Link Href
                            ('#' `T.cons` (objectType ^. common.name))


-- > INDEX COMPONENTS
--------------------------------------------------------------------------------

-- Index Components > Link
--------------------------------------------------------------------------------

indexLinkHTML :: Text -> Text -> Html 
indexLinkHTML linkName linkHref =
  H.a ! A.class_ "index-link" 
      ! (A.href $ toValue linkHref)
      $ toHtml linkName 


-- Index Components > Headers
--------------------------------------------------------------------------------

indexSectionHeaderHTML :: Text -> Html
indexSectionHeaderHTML headerText = H.h5 $ toHtml headerText


indexSectionSubHeaderHTML :: Text -> Html
indexSectionSubHeaderHTML subheaderText = H.h6 $ toHtml subheaderText


-- CONTENT
--------------------------------------------------------------------------------

-- | The HTML for the content section which is the majority of the document 
-- (everything to the right sidebar)
contentHTML :: Spec -> Html
contentHTML spec = typesSectionHTML spec


typesSectionHTML :: Spec -> Html
typesSectionHTML spec = 
  H.div ! A.id "types" $ 
    forM_ (spec ^. types) $ typeContainerHTML spec


typeContainerHTML :: Spec -> ObjectType -> Html
typeContainerHTML spec objectType = do
  H.div ! A.class_ "type" $ do
    H.div ! A.class_ "definition" $ typeHTML spec objectType
    H.div ! A.class_ "example" $ typeExampleHTML


-- TYPE
--------------------------------------------------------------------------------

-- | The HTML represention for a Type
typeHTML :: Spec -> ObjectType -> Html
typeHTML spec objectType = do
  -- Header
  typeHeaderHTML (objectType ^. common.label)
  -- Description
  typeDescriptionHTML (objectType ^. common.description)
  -- Fields / Cases
  case (objectType ^. typeData) of
    Product productType -> typeFieldsHTML spec productType
    Sum     sumType     -> typeCasesHTML sumType


typeHeaderHTML :: Text -> Html
typeHeaderHTML label =
  H.h2 $ toHtml label


typeDescriptionHTML :: Maybe Text -> Html
typeDescriptionHTML (Just desc) = 
  H.p ! A.class_ "description" $ toHtml desc
typeDescriptionHTML Nothing     = return ()
   

typeFieldsHTML :: Spec -> ProductType -> Html
typeFieldsHTML spec productType = do
  H.div ! A.class_ "fields" $ do
    H.h3 "Fields"
    H.ul $ forM_ (productType ^. fields) (fieldListItemHTML spec)


fieldListItemHTML :: Spec -> Field -> Html
fieldListItemHTML spec = H.li . fieldHTML spec


-- Type > Field
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


-- Type > Field > Constraint
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


-- Type > Example
--------------------------------------------------------------------------------

typeExampleHTML :: Html
typeExampleHTML = H.span "example"


-- > CASE
--------------------------------------------------------------------------------

typeCasesHTML :: SumType -> Html
typeCasesHTML sumType = do
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


-- CONSTRAINTS
--------------------------------------------------------------------------------

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


