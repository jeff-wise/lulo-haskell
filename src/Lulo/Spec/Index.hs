

{-| Spec Index

    The Spec Index is a Spec but with many of the fields indexed for faster or 
    more convenient access.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.Spec.Index
  ( SpecIndex
  , specIndex
  , specVersion
  , specDescription
  , specConstraintWithName
  , specCustomTypes
  , specTypesByGroup
  ) where


import Lulo.Spec.Types

import Control.Lens

import Data.Foldable (foldl')
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HML (
    empty
  , insert, insertWith
  , lookup
  , elems
  , toList
  )
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set (
    singleton
  , union
  )
import Data.Maybe (fromMaybe)



--------------------------------------------------------------------------------
-- INDEX
--------------------------------------------------------------------------------

-- | An indexed Spec so that provides a nicer and more efficient interface for 
-- looking up components of a specification.
data SpecIndex = SpecIndex
  { specIndexVersion           :: SpecVersion
  , specIndexAuthors           :: [SpecAuthor]
  , specIndexDescription       :: Maybe SpecDescription    
  , specIndexRootTypeName      :: Maybe CustomTypeName    
  , specIndexTypeByName        :: HashMap CustomTypeName CustomType
  , specIndexConstraintByName  :: HashMap ConstraintName Constraint
  , specIndexTypesByGroup      :: HashMap CustomTypeGroup (HashSet CustomType)
  }


-- Create
--------------------------------------------------------------------------------

-- | Create a new spec index
specIndex :: Spec -> SpecIndex
specIndex spec = 
  SpecIndex
    (spec ^. version)
    (spec ^. authors)
    (spec ^. description)
    (spec ^. rootTypeName)
    (customTypeByNameMap $ spec ^. types)
    (constraintByNameMap $ spec ^. constraints)
    (groupToTypesMap $ spec ^. types)

  where
                
    -- | CustomTypeName -> CustomType index
    customTypeByNameMap :: [CustomType] -> HashMap CustomTypeName CustomType
    customTypeByNameMap = foldl' withCustomType HML.empty
      where
        withCustomType hm t = HML.insert (t ^. typeData.name) t hm

    -- | ConstraintName -> CustomType index
    constraintByNameMap :: [Constraint] -> HashMap ConstraintName Constraint
    constraintByNameMap = foldl' indexConstraint HML.empty
      where
        indexConstraint hm c = HML.insert (c ^. constraintData.name) c hm

    -- | Group -> Custom Type index
    -- Custom type can be put into groups to organize them. For some group, get 
    -- all of the types in that group.
    groupToTypesMap :: [CustomType] -> HashMap CustomTypeGroup (HashSet CustomType)
    groupToTypesMap = foldl' withCustomType HML.empty 
      where
        withCustomType hm _customType = 
          let groupName = fromMaybe (CustomTypeGroup "no_group") 
                                    (_customType ^. typeData.group)
          in  HML.insertWith Set.union groupName (Set.singleton _customType) hm


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

specVersion :: SpecIndex -> SpecVersion
specVersion = specIndexVersion 


specDescription :: SpecIndex -> Maybe SpecDescription
specDescription = specIndexDescription


specConstraintWithName :: SpecIndex -> ConstraintName -> Maybe Constraint
specConstraintWithName = flip HML.lookup . specIndexConstraintByName


specCustomTypes :: SpecIndex -> [CustomType]
specCustomTypes = HML.elems . specIndexTypeByName 


specTypesByGroup :: SpecIndex -> [(CustomTypeGroup, HashSet CustomType)]
specTypesByGroup = HML.toList . specIndexTypesByGroup


-- specConstraint :: Spec -> ConstraintName -> Maybe LuloConstraint
-- specConstraint spec constraintName =
--       builtInConstraint constraintName
--   <|> HML.lookup constraintName (_specConstraintIndex spec)


