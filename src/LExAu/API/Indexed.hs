{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- |Interface module for Indexed Collections.
module LExAu.API.Indexed (
    Collection(allMembers),
    Indexed(allIndices,maybeMember,member,(!)),
    IndexedMap(get),
    IndexedMember(index)
  ) where

-- |Methods for collections.
class
    Collection collectionType memberType
    | collectionType -> memberType, memberType -> collectionType
    where
  allMembers :: collectionType -> [memberType]

-- |Methods for collections that contain members that know their indices in the collection.
class (
      Collection collectionType memberType,
      IndexedMember memberType indexType,
      Show collectionType,
      Show indexType
    ) =>
    Indexed collectionType memberType indexType
    | collectionType -> memberType
    where
  -- |Returns the member for the given index.
  member :: collectionType -> indexType -> memberType
  member collection idx =
    case maybeMember collection idx of
      Just member -> member
      Nothing -> error $ (showString "Member not found: " . shows collection . showChar '[' . shows idx) "]"
  maybeMember :: collectionType -> indexType -> Maybe memberType

  -- |Returns a list of the indices of all members of the given collection.
  allIndices :: collectionType -> [indexType]

  -- |Alias for the member method.
  (!) :: collectionType -> indexType -> memberType
  (!) = member

-- |Methods for maps that use an indexed collection as its domain.
class
    (IndexedMember memberType indexType) =>
    IndexedMap mapType memberType indexType valueType
    where
  -- |Returns the value associated with the given member.
  get :: mapType -> memberType -> valueType

-- |Methods for members that know their indices in the collection.
class
    IndexedMember memberType indexType
    where
  -- |Returns the index of the given member in the collection it belongs to.
  index :: memberType -> indexType
