
{-| Content

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML.Spec.Content where


import Lulo.HTML.Spec.Combinators (objectId)
import Lulo.Schema.Index (
    SchemaIndex
  , schemaIndexDescription
  , constraintWithName
  , typesByGroupAsc
  , schemaIndexMetadata
  )
import Lulo.Schema.Types

import Control.Monad (unless)

import Data.Char (toLower)
import Data.HashSet (HashSet)
import Data.Foldable (forM_)
import Data.Monoid
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T (toLower)
import qualified Data.Text.Lazy as LT (fromStrict)

import Text.Blaze.Html5 (
    Html
  , toHtml
  , (!)
  )
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Text.Markdown (markdown, defaultMarkdownSettings)



html :: SchemaIndex -> Html
html schemaIndex = do
  descriptionHtml (schemaName $ schemaIndexMetadata schemaIndex) 
                  (schemaIndexDescription schemaIndex)
  typesHtml schemaIndex


--------------------------------------------------------------------------------
-- DESCRIPTION
--------------------------------------------------------------------------------

descriptionHtml :: SchemaName -> Maybe SchemaDescription -> Html 
descriptionHtml (SchemaName _schemaName) mSchemaDesc =
  H.div ! A.id "description" $ do
    H.div ! A.class_ "definition" $ do
      H.div ! A.class_ "title" $
        H.h1 $ toHtml (_schemaName <> " Schema")
      H.div ! A.class_ "introduction" $ do
        H.h2 ! A.id "introduction-overview" $ "Overview"
        case mSchemaDesc of
          Just schemaDesc ->
            H.div ! A.class_ "overview" $
              markdown defaultMarkdownSettings $ LT.fromStrict (descOverviewMarkdown schemaDesc)
          Nothing         -> return ()
    H.div ! A.class_ "example" $ return ()


--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

typesHtml :: SchemaIndex -> Html
typesHtml specIndex =
  H.div ! A.id "types" $ 
    forM_ (typesByGroupAsc specIndex) $ 
      uncurry $ typeGroupHtml specIndex


-- Types > Group
--------------------------------------------------------------------------------

typeGroupHtml :: SchemaIndex -> CustomTypeGroup -> HashSet CustomType -> Html
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

typeHtml :: SchemaIndex -> CustomType -> Html
typeHtml specIndex customType =
  containerDiv $ do
    H.div ! A.class_ "definition" $ typeDataHtml specIndex customType
    H.div ! A.class_ "example" $ typeExampleHTML
  where
    containerDiv = H.div ! A.id (objectId $ getCustomTypeName $ 
                                            typeName customType)
                         ! A.class_ "type"

typeDataHtml :: SchemaIndex -> CustomType -> Html
typeDataHtml specIndex _type = do
  -- Header
  typeHeaderHtml (getCustomTypeLabel $ typeLabel _type)
  -- Fields / Cases
  case _type of
    CustomTypeProduct productType -> typeFieldsHtml specIndex productType
    CustomTypeSum     sumType     -> typeCasesHtml sumType
    CustomTypePrim    synType     -> typeSynonymHtml synType specIndex


typeHeaderHtml :: Text -> Html
typeHeaderHtml = H.h3 . toHtml


typeDescriptionHtml :: CustomTypeDescription -> Html
typeDescriptionHtml (CustomTypeDescription desc) = 
  H.p ! A.class_ "description" $ toHtml desc
   

-- TYPES > TYPE > FIELD
--------------------------------------------------------------------------------

typeFieldsHtml :: SchemaIndex -> ProductCustomType -> Html
typeFieldsHtml specIndex productType = do
  -- Description
  case prodTypeDescription productType of
    Just    desc -> typeDescriptionHtml desc
    Nothing      -> return ()
  H.div ! A.class_ "fields" $ do
    let numberOfFields = length $ prodTypeFields productType
    H.h4 $ toHtml $ show numberOfFields <> " Fields"
    H.ul $ forM_ (prodTypeFields productType) (H.li . fieldHtml specIndex)


fieldHtml :: SchemaIndex -> Field -> Html
fieldHtml schemaIndex field =
  fieldContainerDiv $ do
    fieldPresenceHtml $ fieldPresence field
    H.header $ do
      fieldNameHtml $ fieldName field
      fieldTypeHtml $ fieldValueType field
    fieldDescriptionHtml $ fieldDescription field
    constraintsHtml (fieldConstraints field) schemaIndex
  where
    fieldContainerDiv = H.div ! A.class_ "field"


-- Types > Type > Field > Presence
--------------------------------------------------------------------------------

fieldPresenceHtml :: FieldPresence -> Html
fieldPresenceHtml Optional =
  --let presenceClassString = "presence " ++ map toLower (show presence)
  H.div ! A.class_ "presence" $ "optional"
fieldPresenceHtml Required = return ()


-- Types > Type > Field > Name
--------------------------------------------------------------------------------

fieldNameHtml :: FieldName -> Html
fieldNameHtml name = 
  H.div ! A.class_ "name" $ toHtml $ getFieldName name


-- Types > Type > Field > Type
--------------------------------------------------------------------------------

fieldTypeHtml :: ValueType -> Html
fieldTypeHtml fieldType = 
  H.div ! A.class_ "type" $
    H.a $ toHtml (map toLower $ show fieldType)


-- Types > Type > Field > Description
--------------------------------------------------------------------------------

fieldDescriptionHtml :: Maybe FieldDescription -> Html
fieldDescriptionHtml (Just desc) = 
  H.div ! A.class_ "description" $
    toHtml $ getFieldDesc desc
fieldDescriptionHtml Nothing     = return ()


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


-- Constraints
--------------------------------------------------------------------------------

constraintsHtml :: [ConstraintName] -> SchemaIndex -> Html
constraintsHtml constraintNames specIndex = do
  let consts = catMaybes $ fmap (constraintWithName specIndex) 
                                constraintNames 
  unless (null consts) $
    H.ul ! A.class_ "constraints" $ 
      mapM_ fieldConstraintListItemHTML consts


fieldConstraintListItemHTML :: Constraint -> Html
fieldConstraintListItemHTML = H.li . fieldConstraintHTML


fieldConstraintHTML :: Constraint -> Html
fieldConstraintHTML (ConstraintStringOneOf    c) = stringOneOfConstraintHTML c
fieldConstraintHTML (ConstraintNumGreaterThan c) = numGreaterThanConstraintHTML c


-- TYPES > TYPE > CASE
--------------------------------------------------------------------------------

typeCasesHtml :: SumCustomType -> Html
typeCasesHtml sumType = do
  -- Description
  case sumTypeDescription sumType of
    Just    desc -> typeDescriptionHtml desc
    Nothing      -> return ()
  H.div ! A.class_ "cases" $ do
    H.h4 "Cases"
    H.ul $ forM_ (sumTypeCases sumType) (H.li . caseHtml)


caseHtml :: Case -> Html
caseHtml sumCase =
  H.div ! A.class_ "case" $ do
    -- Type
    H.div ! A.class_ "case-type" $ 
      H.a $ toHtml (T.toLower $ getCustomTypeName $ caseType sumCase)
    -- Description
    case caseDescription sumCase of
      Just desc -> 
        H.div ! A.class_ "case-description" $
          H.p $ toHtml $ getCaseDescription desc
      Nothing              -> return ()


-- Types > Synonym
--------------------------------------------------------------------------------

typeSynonymHtml :: PrimCustomType -> SchemaIndex -> Html
typeSynonymHtml synType schemaIndex =
  H.div ! A.class_ "synonym" $ do
    H.h4 $ do
      H.span ! A.class_ "extends" $ "extends"
      H.span ! A.class_ "base-type" $ toHtml $ 
        baseTypeName $ primTypeBaseType synType
    -- Description
    case primTypeDescription synType of
      Just    desc -> typeDescriptionHtml desc
      Nothing      -> return ()
    constraintsHtml (primTypeConstraints synType) schemaIndex


-- TYPES > TYPE > EXAMPLE
--------------------------------------------------------------------------------

typeExampleHTML :: Html
typeExampleHTML = H.span "example"



-- CONSTRAINTS
--------------------------------------------------------------------------------

stringOneOfConstraintHTML :: StringOneOfConstraint -> Html
stringOneOfConstraintHTML oneOf =
  H.div ! A.class_ "constraint constraint-string-one-of" $ do
    H.h4 ! A.class_ "constraint" $ "Must be one of"
    H.ul $ 
      forM_ (stringOneOfSet oneOf) $ \(StringOneOfValue val mDesc) ->
        H.li $ do
          H.span ! A.class_ "value" $ toHtml val
          case mDesc of
            Just desc -> H.span ! A.class_ "description" $ toHtml desc
            Nothing   -> return ()


numGreaterThanConstraintHTML :: NumGreaterThanConstraint -> Html
numGreaterThanConstraintHTML greaterThan = 
  H.div ! A.class_ "constraint constraint-number-greater-than" $ do
    H.span "> "
    H.span $ toHtml (numberGreaterThanLowerBound greaterThan)


