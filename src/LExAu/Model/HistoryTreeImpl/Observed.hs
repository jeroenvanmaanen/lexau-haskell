{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module LExAu.Model.HistoryTreeImpl.Observed
  ( HistoryTreeModelImpl(..)
  , ReadModelContext(..)
  , createHistoryTreeModel
  , createEmptyHistoryTreeModel
  ) where

import Data.IntMap (IntMap)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Text.Show (showListWith)
import qualified Data.IntMap as IntMap (assocs, fromList, insert, lookup)

import LExAu.API.Alphabet
  ( Alphabet
  , UpdatableAlphabet(possiblyNewerThan)
  )
import LExAu.API.HistoryTree
  ( HistoryTreeModel
      ( clearUpdated
      , increaseHistoryTree
      , increaseHistoryTreeST
      , incrementHistoryTree
      , incrementHistoryTreeST
      , markExtensible
      , markExtensibleST
      , maximumDepth
      , updated
      )
  , UpdateMarkers(UpdateMarkers)
  )
import LExAu.API.Distribution
  ( DistributionFactory(DistributionFactory)
  , DistributionType
  , Histogram(increase)
  , LogDistribution(logDistribution)
  )
import LExAu.API.Indexed (IndexedMember, index)
import LExAu.API.Model
  ( AlphabetUpdate(AlphabetUpdate)
  , Updater(update)
  )
import LExAu.API.Named (Named)
import LExAu.API.ReaderContext (ReaderContext(readsWithContext))
import LExAu.Model.HistoryTreeImpl.HTData
  ( HTData(..)
  , HTStatus(..)
  , statusChar
  )
import LExAu.Model.HistoryTreeImpl.Utilities
  ( LogEntry(logEntry)
  , modifiedChar
  )
import LExAu.Utilities.Logging (Loggable(logs))

data HistoryTreeModelImpl alphabetType symbolType distributionType =
  HistoryTreeModelImpl {
    theAlphabet :: alphabetType,
    theUpdatedEntries :: Maybe UpdateMarkers,
    theFactory :: DistributionFactory distributionType,
    theData :: HTData distributionType (),
    theMaxDepth :: Int
  } deriving (Show, Read)

data ReadModelContext alphabetType symbolType distributionType =
  ReadModelContext
    (String -> [String] -> alphabetType)
    (DistributionFactory distributionType -> alphabetType -> (HistoryTreeModelImpl alphabetType symbolType distributionType))
    (DistributionFactory distributionType)

instance (
        Read alphabetType,
        Read distributionType
      ) =>
    ReaderContext (ReadModelContext alphabetType symbolType distributionType) (HistoryTreeModelImpl alphabetType symbolType distributionType) where
  readsWithContext (ReadModelContext alphabetFactory histogramFactory distributionFactory) s =
    map (\(m, t) -> (m { theFactory = distributionFactory }, t)) (reads s)

createHistoryTreeModel ::
      ( Histogram alphabetType symbolType distributionType
      , Alphabet alphabetType symbolType
      ) =>
    (DistributionFactory distributionType)
    -> alphabetType
    -> (DistributionType distributionType)
    -> (HistoryTreeModelImpl alphabetType symbolType distributionType)
createHistoryTreeModel factory alphabet distribution =
  HistoryTreeModelImpl alphabet Nothing factory (HTData distribution (IntMap.fromList []) Extensible ()) 0

createEmptyHistoryTreeModel :: (Histogram alphabetType symbolType distributionType, Alphabet alphabetType symbolType) => (DistributionFactory distributionType) -> alphabetType -> (HistoryTreeModelImpl alphabetType symbolType distributionType)
createEmptyHistoryTreeModel factory@(DistributionFactory (Just create)) alphabet =
  createHistoryTreeModel factory alphabet (create [])

instance (
        LogDistribution alphabetType symbolType distributionType,
        Named symbolType
      ) =>
    Loggable (HistoryTreeModelImpl alphabetType symbolType distributionType) where
  logs historyTreeModel =
    let modelData = theData historyTreeModel
        symbols = theAlphabet historyTreeModel
        updated = theUpdatedEntries historyTreeModel
        distribution = theDistribution modelData
        childNodes = theChildren modelData
    in showString "{HistoryTreeModel: maxDepth=" . shows (theMaxDepth historyTreeModel) .
       showString ", flags='" . showChar (modifiedChar updated) . showChar (statusChar modelData) . showChar '\'' .
       showString "\n  " . shows symbols .
       showString "\n  " . logDistribution symbols distribution .
       showString "\n   " .
       (showListWith (logEntry symbols updated "  ") $ IntMap.assocs childNodes) .
       showChar '}'

increaseHistoryTreeData :: (
        Histogram alphabetType symbolType distributionType
      ) =>
    alphabetType ->
    DistributionFactory distributionType ->
    (HTData distributionType ()) ->
    (Maybe UpdateMarkers) ->
    Int ->
    [symbolType] ->
    symbolType ->
    Integer ->
      ((HTData distributionType ()), IntMap UpdateMarkers, Int)

increaseHistoryTreeData alphabet factory oldData oldMaybeUpdated depth [] nextResponse amount =
  let oldDistribution = theDistribution oldData
      newDistribution = increase oldDistribution nextResponse amount
      newUpdatedMap =
        case oldMaybeUpdated of
          Just (UpdateMarkers oldUpdatedMap) -> oldUpdatedMap
          Nothing -> IntMap.fromList []
      newData = oldData { theDistribution = newDistribution }
   in (newData, newUpdatedMap, depth)

increaseHistoryTreeData alphabet factory oldData oldMaybeUpdated depth (symbol : symbols) nextResponse amount =
  let oldDistribution = theDistribution oldData
      newDistribution = increase oldDistribution nextResponse amount
      locallyUpdatedData = oldData { theDistribution = newDistribution }
      oldUpdatedMap =
        case oldMaybeUpdated of
          Just (UpdateMarkers updatedMap) -> updatedMap
          Nothing -> IntMap.fromList []
  in case theStatus oldData of
       Extensible ->
         let key = index symbol
             oldChildren = theChildren oldData
             maybeOldChild = key `IntMap.lookup` oldChildren
             oldChild =
               case maybeOldChild of
                 Nothing ->
                   let DistributionFactory (Just create) = factory
                   in let newStatus = if depth < 3 then Extensible else Barrier
                      in (HTData (create []) (IntMap.fromList []) newStatus ())
                 Just child -> child
             oldUpdatedChild =
               case key `IntMap.lookup` oldUpdatedMap of
                 Just child -> child
                 Nothing -> UpdateMarkers (IntMap.fromList [])
             (newChild, newUpdatedChildMap, childDepth) = increaseHistoryTreeData alphabet factory oldChild (Just oldUpdatedChild) (depth + 1) symbols nextResponse amount
             newChildren = IntMap.insert key newChild oldChildren
             newData = locallyUpdatedData { theChildren = newChildren }
             newUpdatedMap = IntMap.insert key (UpdateMarkers newUpdatedChildMap) oldUpdatedMap
         in (newData, newUpdatedMap, childDepth)
       Barrier -> (locallyUpdatedData, oldUpdatedMap, depth)

markHistoryTreeDataExtensible :: (IndexedMember symbolType Int) => (HTData distributionType extraPropertiesType) -> [symbolType] -> (HTData distributionType extraPropertiesType)
markHistoryTreeDataExtensible oldData [] =
  oldData { theStatus = Extensible }

markHistoryTreeDataExtensible oldData (symbol : symbols) =
  let key = index symbol
      oldChildren = theChildren oldData
      maybeOldChild = key `IntMap.lookup` oldChildren
      newData =
        case maybeOldChild of
          Just oldChild ->
            let newChild = markHistoryTreeDataExtensible oldChild symbols
                newChildren = IntMap.insert key newChild oldChildren
            in oldData { theStatus = Extensible, theChildren = newChildren }
          Nothing -> oldData { theStatus = Extensible }
  in newData

instance (
      Alphabet alphabetType symbolType,
      Histogram alphabetType symbolType distributionType
    ) =>
    HistoryTreeModel (HistoryTreeModelImpl alphabetType symbolType distributionType) symbolType where

  increaseHistoryTree model history nextResponse amount =
    let alphabet = theAlphabet model
        factory = theFactory model
        oldData = theData model
        oldMaybeUpdated = theUpdatedEntries model
        oldMaxDepth = theMaxDepth model
        (newData, newUpdatedMap, depth) = increaseHistoryTreeData alphabet factory oldData oldMaybeUpdated 0 history nextResponse amount
        newMaxDepth = max oldMaxDepth depth
    in model { theUpdatedEntries = Just (UpdateMarkers newUpdatedMap), theData = newData, theMaxDepth = newMaxDepth }

  increaseHistoryTreeST historyTreeRef history nextResponse amount =
    do oldModel <- readSTRef historyTreeRef
       newModel <- return $ increaseHistoryTree oldModel history nextResponse amount
       writeSTRef historyTreeRef newModel
       return ()

  markExtensible model history =
    let oldData = theData model
        newData = markHistoryTreeDataExtensible oldData history
    in model { theData = newData }

  markExtensibleST historyTreeRef history =
    do oldModel <- readSTRef historyTreeRef
       newModel <- return $ markExtensible oldModel history
       writeSTRef historyTreeRef newModel
       return ()

  maximumDepth model = theMaxDepth model

  updated model = theUpdatedEntries model

  clearUpdated model = model { theUpdatedEntries = Nothing }

instance (
      Alphabet alphabetType symbolType,
      UpdatableAlphabet alphabetType
    ) =>
    Updater AlphabetUpdate (HistoryTreeModelImpl alphabetType symbolType distributionType) alphabetType where
  update AlphabetUpdate newAlphabet oldModel =
    if newAlphabet `possiblyNewerThan` (theAlphabet oldModel)
      then Just $ oldModel { theAlphabet = newAlphabet }
      else Nothing
