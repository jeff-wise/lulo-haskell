
{-| Content

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML.Spec.Content where


import Lulo.HTML.Spec.Combinators (objectId)
import Lulo.Schema.Index (
    SchemaIndex
  , schemaIndexDescription
  , schemaIndexExampleLanguages
  , constraintWithName
  , typesByGroupAsc
  , schemaIndexMetadata
  )
import Lulo.Schema.Types

import Control.Monad (unless)

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS (toList)
import Data.Foldable (forM_)
import Data.List (sortOn)
import Data.Monoid
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T (cons, toLower)
import qualified Data.Text.Lazy as LT (fromStrict)

import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html5 
  ( Html
  , toHtml, toValue
  , (!)
  )
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H
import Text.Markdown (markdown, defaultMarkdownSettings)



html :: SchemaIndex -> Html
html schemaIndex = do
  headerHtml schemaIndex
  descriptionHtml (schemaName $ schemaIndexMetadata schemaIndex) 
                  (schemaIndexDescription schemaIndex)
  typesHtml schemaIndex


--------------------------------------------------------------------------------
-- HEADER
--------------------------------------------------------------------------------

headerHtml :: SchemaIndex -> Html
headerHtml schemaIndex = 
  H.header ! A.class_ "languages" $ do
    H.div ! A.class_ "definition" $ return ()
    H.div ! A.class_ "display" $ do
      let languages = schemaIndexExampleLanguages schemaIndex 
      H.div ! A.class_ "buttons" $ 
        forM_ languages languageButtonHtml


languageButtonHtml :: Text -> Html
languageButtonHtml language = do
  let classes = "button" <> (if T.toLower language == "yaml" then " selected" else "") :: Text
  H.div ! A.class_ (toValue classes) $ toHtml language


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
    H.div ! A.class_ "display" $ return ()


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
typeGroupHtml specIndex _group customTypeSet = do
  typeGroupHeaderHtml _group
  let sortedTypes = sortOn typeName $ HS.toList customTypeSet
  H.div ! A.class_ "group" $
    forM_ sortedTypes $ typeHtml specIndex


typeGroupHeaderHtml :: CustomTypeGroup -> Html
typeGroupHeaderHtml (CustomTypeGroup _group) =
  containerDiv $ do
    H.div ! A.class_ "definition" $
      H.h2 ! A.id (objectId _group) $ toHtml _group
    H.div ! A.class_ "display" $ ""
  where
    containerDiv = H.div ! A.class_ "section-header"

-- Types > Type
--------------------------------------------------------------------------------

typeHtml :: SchemaIndex -> CustomType -> Html
typeHtml schemaIndex customType =
  containerDiv $ do
    H.div ! A.class_ "definition" $ typeDataHtml schemaIndex customType
    H.div ! A.class_ "display" $ typeExampleHtml (typeCodeExamples customType) "yaml"
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
    CustomTypeSymbol  _           -> return ()


typeHeaderHtml :: Text -> Html
typeHeaderHtml _typeName = H.h3 ! A.id (objectId _typeName) $ toHtml _typeName


typeDescriptionHtml :: CustomTypeDescription -> Html
typeDescriptionHtml (CustomTypeDescription desc) = 
  H.div ! A.class_ "description" $
    markdown defaultMarkdownSettings $ LT.fromStrict desc
   

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
    case fieldType of
      Prim primValueType -> H.span ! A.class_ "primitive" $ toHtml $ show primValueType
      PrimList primValueType ->
        H.div ! A.class_ "primitive" $ do
          H.span "list of"
          H.span $ toHtml $ show primValueType
      Custom customTypeName -> do
        let _typeName = getCustomTypeName customTypeName
        H.a ! A.href (toValue $ T.cons '#' _typeName) $ toHtml _typeName
      CustomList customTypeName -> do
        let _typeName = getCustomTypeName customTypeName
        H.div $ do
          H.span "list of"
          H.a ! A.href (toValue $ T.cons '#' _typeName) $ toHtml _typeName



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
    let numberOfCases = length $ sumTypeCases sumType
    H.h4 $ toHtml $ show numberOfCases <> " Cases"
    H.ul $ forM_ (sumTypeCases sumType) (H.li . caseHtml)


caseHtml :: Case -> Html
caseHtml sumCase =
  H.div ! A.class_ "case" $ do
    -- Type
    caseTypeHtml $ caseType sumCase
    -- Description
    case caseDescription sumCase of
      Just desc -> 
        H.div ! A.class_ "case-description" $
          H.p $ toHtml $ getCaseDescription desc
      Nothing              -> return ()


caseTypeHtml :: CustomTypeName -> Html
caseTypeHtml customTypeName = 
  H.div ! A.class_ "type" $ do
    let _typeName = getCustomTypeName customTypeName
    H.a ! A.href (toValue $ T.cons '#' _typeName) $ toHtml _typeName

    
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

typeExampleHtml :: [CodeExample] -> Text -> Html
typeExampleHtml codeExamples defaultLanguage = 
  H.div ! A.class_ "examples" $ do
    H.h3 "Examples"
    useCaseHtml codeExamples defaultLanguage


useCaseHtml :: [CodeExample] -> Text -> Html
useCaseHtml codeExamples defaultLanguage = 
  H.div ! A.class_ "use_case" $
    forM_ codeExamples $ exampleHtml defaultLanguage


exampleHtml :: Text -> CodeExample -> Html
exampleHtml defaultLanguage codeExample = do
  let language = T.toLower $ codeExampleLanguage codeExample
      classes = language
                <> " example"
                <> (if defaultLanguage == language then " selected" else "")
  H.div ! A.class_ (toValue classes) $ do
    H.h4 $ toHtml $ codeExampleTitle codeExample
    case codeExampleDescription codeExample of
      Just desc -> H.p $ toHtml desc
      Nothing   -> return ()
    --H.pre $ H.code $ toHtml $ codeExampleCode codeExample 
    preEscapedToHtml $ "<pre><code>" <> codeExampleCode codeExample <> "</pre></code>"


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


