

{-| schema Index

    The schema Index is a schema but with many of the fields indexed for faster or 
    more convenient access.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Lulo.Schema.Index
  ( SchemaIndex
  , schemaIndex
  , schemaIndexVersion
  , schemaIndexMetadata
  , schemaIndexDescription
  , schemaIndexRootTypeName
  , schemaIndexExampleLanguages
  , constraintWithName
  , sortedConstraintsASC
  , customTypes
  , typesByGroupAsc
  , typeWithName
  ) where


import Lulo.Schema.Types

import Control.Category ((>>>))
import Control.Monad (msum)

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
import qualified Data.HashSet as Set
  ( singleton, empty
  , insert
  , union
  )
import Data.Maybe (fromMaybe)

import Data.Text (Text)


-- TODO why not using hashmap? 

--------------------------------------------------------------------------------
-- INDEX
--------------------------------------------------------------------------------

-- | An indexed schema so that provides a nicer and more efficient interface for 
-- looking up components of a schemaification.
data SchemaIndex = SchemaIndex
  { schemaIndexVersion           :: SchemaVersion
  , schemaIndexMetadata          :: SchemaMetadata
  , schemaIndexDescription       :: Maybe SchemaDescription    
  , schemaIndexRootTypeName      :: CustomTypeName    
  , schemaIndexTypeByName        :: Map CustomTypeName CustomType
  , schemaIndexConstraintByName  :: Map ConstraintName Constraint
  , schemaIndexTypesByGroup      :: Map CustomTypeGroup (HashSet CustomType)
  , schemaIndexExampleLanguages  :: HashSet Text
  }


-- Create
--------------------------------------------------------------------------------

-- | Create a new schema index
schemaIndex :: Schema -> SchemaIndex
schemaIndex schema = 
  SchemaIndex
    (schemaVersion schema)
    (schemaMetadata schema)
    (schemaDescription schema)
    (schemaRootTypeName schema)
    (customTypeByNameMap $ schemaTypes schema)
    (constraintByNameMap $ schemaConstraints schema)
    (groupToTypesMap $ schemaTypes schema)
    (exampleLanguageSet $ schemaTypes schema)

  where
                
    -- | CustomTypeName -> CustomType index
    customTypeByNameMap :: [CustomType] -> Map CustomTypeName CustomType
    customTypeByNameMap = foldl' withCustomType Map.empty
      where
        withCustomType hm t = Map.insert (typeName t) t hm

    -- | ConstraintName -> CustomType index
    constraintByNameMap :: [Constraint] -> Map ConstraintName Constraint
    constraintByNameMap = foldl' indexConstraint Map.empty
      where
        indexConstraint hm c = Map.insert (constraintName c) c hm

    -- | Group -> Custom Type index
    -- Custom type can be put into groups to organize them. For some group, get 
    -- all of the types in that group.
    groupToTypesMap :: [CustomType] -> Map CustomTypeGroup (HashSet CustomType)
    groupToTypesMap = foldl' withCustomType Map.empty 
      where
        withCustomType hm _customType = 
          let groupName = fromMaybe (CustomTypeGroup "no_group") 
                                    (typeGroup _customType)
          in  Map.insertWith Set.union groupName (Set.singleton _customType) hm

    exampleLanguageSet :: [CustomType] -> HashSet Text
    exampleLanguageSet = fmap typeLanguages
                     >>> msum
                     >>> foldl' (flip Set.insert) Set.empty
      where
        typeLanguages = fmap codeExampleLanguage . typeCodeExamples
                         -- fmap (fmap codeExampleLanguage . typeCodeExamples)
                     -- >>> msum
                     -- >>> foldl' (flip Set.insert) Set.empty


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

constraintWithName :: SchemaIndex -> ConstraintName -> Maybe Constraint
constraintWithName = flip Map.lookup . schemaIndexConstraintByName


customTypes :: SchemaIndex -> [CustomType]
customTypes = Map.elems . schemaIndexTypeByName 


typesByGroupAsc :: SchemaIndex -> [(CustomTypeGroup, HashSet CustomType)]
typesByGroupAsc = Map.toAscList . schemaIndexTypesByGroup


sortedConstraintsASC :: SchemaIndex -> [Constraint]
sortedConstraintsASC = fmap snd . Map.toAscList . schemaIndexConstraintByName


typeWithName :: SchemaIndex -> CustomTypeName -> Maybe CustomType
typeWithName _schemaIndex _typeName = 
  Map.lookup _typeName (schemaIndexTypeByName _schemaIndex)
