
{-| Content

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML.Spec.Content where


import Lulo.HTML.Spec.Combinators (objectId)
import Lulo.Spec.Index (
    SpecIndex
  , specIndexDescription
  , constraintWithName
  , typesByGroupAsc
  )
import Lulo.Spec.Types

import Control.Monad (unless)

import Data.Char (toLower)
import Data.HashSet (HashSet)
import Data.Foldable (forM_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T (toLower)
import qualified Data.Text.Lazy as LT (fromStrict)

import Text.Blaze.Html5 (
    Html
  , toHtml, toValue
  , (!)
  )
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Text.Markdown (markdown, defaultMarkdownSettings)



html :: SpecIndex -> Html
html specIndex = do
  maybe (return ()) descriptionHtml $ specIndexDescription specIndex
  typesHtml specIndex


--------------------------------------------------------------------------------
-- DESCRIPTION
--------------------------------------------------------------------------------

descriptionHtml :: SpecDescription -> Html 
descriptionHtml desc =
  H.div ! A.id "description" $ do
    H.div ! A.class_ "definition" $ do
      H.h2 "Introduction"
      H.div ! A.class_ "overview" $
        markdown defaultMarkdownSettings $ LT.fromStrict (descOverviewMarkdown desc)
    H.div ! A.class_ "example" $ return ()


--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

typesHtml :: SpecIndex -> Html
typesHtml specIndex =
  H.div ! A.id "types" $ 
    forM_ (typesByGroupAsc specIndex) $ 
      uncurry $ typeGroupHtml specIndex


-- Types > Group
--------------------------------------------------------------------------------

typeGroupHtml :: SpecIndex -> CustomTypeGroup -> HashSet CustomType -> Html
typeGroupHtml specIndex _group customTypes = do
  typeGroupHeaderHtml _group
  H.div ! A.class_ "group" $
    forM_ customTypes $ typeHtml specIndex


typeGroupHeaderHtml :: CustomTypeGroup -> Html
typeGroupHeaderHtml (CustomTypeGroup _group) =
  containerDiv $ do
    H.div ! A.class_ "definition" $
      H.h2 ! A.id (objectId _group) $ toHtml _group
    H.div ! A.class_ "example" $ ""
  where
    containerDiv = H.div ! A.class_ "section-header"

-- Types > Type
--------------------------------------------------------------------------------

typeHtml :: SpecIndex -> CustomType -> Html
typeHtml specIndex customType =
  containerDiv $ do
    H.div ! A.class_ "definition" $ typeDataHtml specIndex customType
    H.div ! A.class_ "example" $ typeExampleHTML
  where
    containerDiv = H.div ! A.id (objectId $ getCustomTypeName $ 
                                            typeName $ typeData customType)
                         ! A.class_ "type"

typeDataHtml :: SpecIndex -> CustomType -> Html
typeDataHtml specIndex _type = do
  -- Header
  typeHeaderHtml (getCustomTypeLabel $ typeLabel $ typeData _type)
  -- Description
  case typeDescription $ typeData _type of
    Just    desc -> typeDescriptionHtml desc
    Nothing      -> return ()
  -- Fields / Cases
  case customType' _type of
    CustomTypeProduct productType -> typeFieldsHtml specIndex productType
    CustomTypeSum     sumType     -> typeCasesHtml sumType
    CustomTypePrim    _           -> return ()


typeHeaderHtml :: Text -> Html
typeHeaderHtml = H.h3 . toHtml


typeDescriptionHtml :: CustomTypeDescription -> Html
typeDescriptionHtml (CustomTypeDescription desc) = 
  H.p ! A.class_ "description" $ toHtml desc
   

-- TYPES > TYPE > FIELD
--------------------------------------------------------------------------------


typeFieldsHtml :: SpecIndex -> ProductCustomType -> Html
typeFieldsHtml specIndex productType =
  H.div ! A.class_ "fields" $ do
    H.h4 "Fields"
    H.ul $ forM_ (typeFields productType) (H.li . fieldHtml specIndex)


fieldHtml :: SpecIndex -> Field -> Html
fieldHtml specIndex field =
  fieldContainerDiv $ do
    -- Name
    fieldNameHtml $ fieldName field
    -- Presence
    fieldPresenceHtml $ fieldPresence field
    -- Type
    fieldTypeHtml $ fieldValueType field
    -- Description
    fieldDescriptionHtml $ fieldDescription field
    -- Constraints
    fieldConstraintsHtml (fieldConstraints field) specIndex
    -- Default Value
    fieldDefaultValueHtml $ fieldDefaultValue field
  where
    fieldContainerDiv = H.div ! A.class_ "field"


-- Types > Type > Field > Presence
--------------------------------------------------------------------------------

fieldPresenceHtml :: FieldPresence -> Html
fieldPresenceHtml presence = do
  let presenceClassString = "presence value " ++ map toLower (show presence)
      fieldValue = case presence of
                     Required -> "Yes"
                     Optional -> "No"
  H.div ! A.class_ "property" $ do
    H.span ! A.class_ "label" $ "IS REQUIRED?"
    H.div ! A.class_ (toValue presenceClassString) $ fieldValue


-- Types > Type > Field > Name
--------------------------------------------------------------------------------

fieldNameHtml :: FieldName -> Html
fieldNameHtml name = 
  H.div ! A.class_ "property" $ do
    H.span ! A.class_ "label" $ "NAME"
    H.h5 ! A.class_ "name value" $ 
      toHtml (getFieldName name)


-- Types > Type > Field > Type
--------------------------------------------------------------------------------

fieldTypeHtml :: ValueType -> Html
fieldTypeHtml fieldType = 
  H.div ! A.class_ "property" $ do
    H.span ! A.class_ "label" $ "TYPE"
    H.div ! A.class_ "type value" $
      H.a $ toHtml (map toLower $ show fieldType)


-- Types > Type > Field > Description
--------------------------------------------------------------------------------

fieldDescriptionHtml :: Maybe FieldDescription -> Html
fieldDescriptionHtml (Just desc) = 
  H.div ! A.class_ "property" $ do
    H.span ! A.class_ "label" $ "DESCRIPTION"
    H.div ! A.class_ "description value" $
      toHtml $ getFieldDesc desc
fieldDescriptionHtml Nothing                 = return ()


-- Types > Type > Field > Default Value
--------------------------------------------------------------------------------

fieldDefaultValueHtml :: Maybe FieldDefaultValue -> Html
fieldDefaultValueHtml (Just (FieldDefaultValue defValue)) =
  H.div ! A.class_ "property" $ do
    H.span ! A.class_ "label" $ "DEFAULT VALUE"
    H.div ! A.class_ "default-value value" $ do
      H.h4 "Default Value"
      H.span ! A.class_ "default-value" $ toHtml defValue
fieldDefaultValueHtml Nothing                                 = return ()


-- Types > Type > Field > Constraints
--------------------------------------------------------------------------------

fieldConstraintsHtml :: [ConstraintName] -> SpecIndex -> Html
fieldConstraintsHtml constraintNames specIndex = do
  let consts = catMaybes $ fmap (constraintWithName specIndex) 
                                constraintNames 
  unless (null consts) $
    H.div ! A.class_ "constraints" $ do
      H.h4 "Constraints"
      H.ul $ mapM_ fieldConstraintListItemHTML consts


fieldConstraintListItemHTML :: Constraint -> Html
fieldConstraintListItemHTML = H.li . fieldConstraintHTML


fieldConstraintHTML :: Constraint -> Html
fieldConstraintHTML constraint =
  case constraint' constraint of
    StringOneOf    c -> stringOneOfConstraintHTML c
    NumGreaterThan c -> numGreaterThanConstraintHTML c


-- TYPES > TYPE > CASE
--------------------------------------------------------------------------------

typeCasesHtml :: SumCustomType -> Html
typeCasesHtml sumType =
  H.div ! A.class_ "cases" $ do
    H.h3 "Cases"
    H.ul $ forM_ (typeCases sumType) (H.li . caseHtml)


caseHtml :: Case -> Html
caseHtml sumCase = do
  -- Type
  H.div ! A.class_ "case-type" $ 
    toHtml (T.toLower $ getCustomTypeName $ caseType sumCase)
  -- Description
  case caseDescription sumCase of
    Just desc -> 
      H.div ! A.class_ "case-description" $
        H.p $ toHtml $ getCaseDescription desc
    Nothing              -> return ()


-- TYPES > TYPE > EXAMPLE
--------------------------------------------------------------------------------

typeExampleHTML :: Html
typeExampleHTML = H.span "example"



-- CONSTRAINTS
--------------------------------------------------------------------------------

stringOneOfConstraintHTML :: StringOneOfConstraint -> Html
stringOneOfConstraintHTML oneOf =
  H.div ! A.class_ "constraint-string-one-of" $ do
    H.span "Must be one of"
    forM_ (stringOneOfSet oneOf) $ 
      (H.span ! A.class_ "choice") . toHtml


numGreaterThanConstraintHTML :: NumberGreaterThanConstraint -> Html
numGreaterThanConstraintHTML greaterThan = 
  H.div ! A.class_ "constraint-number-greater-than" $ do
    H.span "> "
    H.span $ toHtml (numberGreaterThanLowerBound greaterThan)


