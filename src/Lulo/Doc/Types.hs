
{-| Lulo Document

-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Doc.Types where


import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import Data.Maybe (maybe, listToMaybe)
import Data.Monoid
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T (unpack)



data Doc = 
    DocDict DictDoc
  | DocList ListDoc
  | DocText TextDoc
  | DocNumber NumberDoc
  | DocBoolean BooleanDoc


data DictDoc = DictDoc
  { dictDocFields :: HashMap Text Doc
  , dictDocCases  :: [DocCase] 
  , dictDocPath   :: DocPath
  }


data ListDoc = ListDoc
  { listDocDocs  :: [Doc]
  , listDocCases :: [DocCase] 
  , listDocPath  :: DocPath
  }


data TextDoc = TextDoc
  { textDocValue :: Text
  , textDocCases :: [DocCase] 
  , textDocPath  :: DocPath
  }


data NumberDoc = NumberDoc
  { numberDocValue :: Scientific
  , numberDocCases :: [DocCase] 
  , numberDocPath  :: DocPath
  }


data BooleanDoc = BooleanDoc
  { booleanDocValue :: Bool
  , booleanDocCases :: [DocCase] 
  , booleanDocPath  :: DocPath
  }


docCase :: Doc -> Text
docCase = maybe "" getDocCase . maybeDocCase
  where
    maybeDocCase (DocDict    doc) = listToMaybe $ dictDocCases doc
    maybeDocCase (DocList    doc) = listToMaybe $ listDocCases doc
    maybeDocCase (DocText    doc) = listToMaybe $ textDocCases doc
    maybeDocCase (DocNumber  doc) = listToMaybe $ numberDocCases doc
    maybeDocCase (DocBoolean doc) = listToMaybe $ booleanDocCases doc


docPath :: Doc -> DocPath
docPath (DocDict    doc) = dictDocPath doc
docPath (DocList    doc) = listDocPath doc
docPath (DocText    doc) = textDocPath doc
docPath (DocNumber  doc) = numberDocPath doc
docPath (DocBoolean doc) = booleanDocPath doc


data DocType = 
    DocDictType
  | DocListType
  | DocTextType
  | DocNumberType
  | DocBooleanType


instance Show DocType where
  show DocDictType    = "Dictionary"
  show DocListType    = "List"
  show DocTextType    = "Text"
  show DocNumberType  = "Number"
  show DocBooleanType = "Boolean"


docType :: Doc -> DocType
docType (DocDict    _) = DocDictType
docType (DocList    _) = DocListType
docType (DocText    _) = DocTextType
docType (DocNumber  _) = DocNumberType
docType (DocBoolean _) = DocBooleanType




newtype DocCase = DocCase
  { getDocCase :: Text }
  deriving (Eq, Show)




newtype DocPath = DocPath
  { getDocPath :: [DocNode] }
  deriving (Eq)


pathWithKey :: Text -> DocPath -> DocPath
pathWithKey key (DocPath nodes) = DocPath $ nodes <> [DocKeyNode key]


pathWithIndex :: Int -> DocPath -> DocPath
pathWithIndex idx (DocPath nodes) = DocPath $ nodes <> [DocIndexNode idx]


emptyDocPath :: DocPath
emptyDocPath = DocPath []


instance Show DocPath where
  show (DocPath nodes) = foldl' (\x y -> x <> nodePathSegmentString y) "" nodes


data DocNode = 
    DocKeyNode Text
  | DocIndexNode Int
  deriving (Eq)


nodePathSegmentString :: DocNode -> String
nodePathSegmentString (DocKeyNode   key) = "." <> T.unpack key
nodePathSegmentString (DocIndexNode idx) = "[" <> show idx <> "]"
