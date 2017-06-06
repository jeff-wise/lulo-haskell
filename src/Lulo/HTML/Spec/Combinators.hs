
{-| HTML Combinators

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.HTML.Spec.Combinators where


import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html5 (
    AttributeValue
  , toValue
  )



-- | Create an anchor link from a link URL
anchorLink :: Text -> AttributeValue
anchorLink = toValue . T.cons '#' . objectIdText


objectId :: Text -> AttributeValue
objectId = toValue . objectIdText


objectIdText :: Text -> Text
objectIdText = T.toLower . spacesWithDashes
    

spacesWithDashes :: Text -> Text
spacesWithDashes = T.foldr spaceIsDash T.empty 
  where
    spaceIsDash c t = if c == ' '
                        then '-' `T.cons` t
                        else c `T.cons` t
