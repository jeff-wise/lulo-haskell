

{-| Spec Index

    The Spec Index is a Spec but with many of the fields indexed for faster or 
    more convenient access.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.Spec.Index
  ( SpecIndex
  , specIndex
  , specIndexVersion
  , specIndexMetadata
  , specIndexDescription
  , specIndexRootTypeName
  , constraintWithName
  , customTypes
  , typesByGroupAsc
  ) where


import Lulo.Spec.Types

import Data.Foldable (foldl')
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map (
    empty
  , insert, insertWith
  , lookup
  , elems
  , toAscList
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
  , specIndexMetadata          :: SpecMetadata
  , specIndexDescription       :: Maybe SpecDescription    
  , specIndexRootTypeName      :: Maybe CustomTypeName    
  , specIndexTypeByName        :: Map CustomTypeName CustomType
  , specIndexConstraintByName  :: Map ConstraintName Constraint
  , specIndexTypesByGroup      :: Map CustomTypeGroup (HashSet CustomType)
  }


-- Create
--------------------------------------------------------------------------------

-- | Create a new spec index
specIndex :: Spec -> SpecIndex
specIndex spec = 
  SpecIndex
    (specVersion spec)
    (specMetadata spec)
    (specDescription spec)
    (specRootTypeName spec)
    (customTypeByNameMap $ specTypes spec)
    (constraintByNameMap $ specConstraints spec)
    (groupToTypesMap $ specTypes spec)

  where
                
    -- | CustomTypeName -> CustomType index
    customTypeByNameMap :: [CustomType] -> Map CustomTypeName CustomType
    customTypeByNameMap = foldl' withCustomType Map.empty
      where
        withCustomType hm t = Map.insert (typeName $ typeData t) t hm

    -- | ConstraintName -> CustomType index
    constraintByNameMap :: [Constraint] -> Map ConstraintName Constraint
    constraintByNameMap = foldl' indexConstraint Map.empty
      where
        indexConstraint hm c = Map.insert (constraintName $ constraintData c) c hm

    -- | Group -> Custom Type index
    -- Custom type can be put into groups to organize them. For some group, get 
    -- all of the types in that group.
    groupToTypesMap :: [CustomType] -> Map CustomTypeGroup (HashSet CustomType)
    groupToTypesMap = foldl' withCustomType Map.empty 
      where
        withCustomType hm _customType = 
          let groupName = fromMaybe (CustomTypeGroup "no_group") 
                                    (typeGroup $ typeData _customType)
          in  Map.insertWith Set.union groupName (Set.singleton _customType) hm


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

constraintWithName :: SpecIndex -> ConstraintName -> Maybe Constraint
constraintWithName = flip Map.lookup . specIndexConstraintByName


customTypes :: SpecIndex -> [CustomType]
customTypes = Map.elems . specIndexTypeByName 


typesByGroupAsc :: SpecIndex -> [(CustomTypeGroup, HashSet CustomType)]
typesByGroupAsc = Map.toAscList . specIndexTypesByGroup

