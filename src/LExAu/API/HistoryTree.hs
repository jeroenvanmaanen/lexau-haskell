{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- |Interface module for HistoryTree Chain models.
--
-- Use a factory method in an implementation module of this interface to create
-- instances (e.g., LExAu.Model.HistoryTree.createHistoryTreeModel).
module LExAu.API.HistoryTree (
    HistoryTreeModel(clearUpdated,increaseHistoryTree,increaseHistoryTreeST,incrementHistoryTree,incrementHistoryTreeST,markExtensible,markExtensibleST,maximumDepth,updated),
    UpdateMarkers(UpdateMarkers)
  ) where

import Control.Monad.ST (ST)
import Data.IntMap (IntMap)
import Data.STRef (STRef)

import LExAu.API.Alphabet (Alphabet)
import LExAu.API.Distribution (Distribution,DistributionFactory(DistributionFactory),DistributionType)

-- |A set of markers that indicate updated distributions.
data UpdateMarkers = UpdateMarkers (IntMap UpdateMarkers) deriving (Show, Read)

-- |Methods to update HistoryTree models.
class HistoryTreeModel historyTreeType symbolType | historyTreeType -> symbolType where

  -- |Increases the count for the combination lastResponse-lastAction-nextResponse
  -- by the given amount.
  --
  -- The implementation is of the form:
  -- increaseHistoryTree oldHistoryTreeModel history nextResponse amount = newHistoryTreeModel
  increaseHistoryTree :: historyTreeType -> [symbolType] -> symbolType -> Integer -> historyTreeType

  -- |Increases the count for the combination lastResponse-lastAction-nextResponse
  -- by unity.
  --
  -- The implementation is of the form:
  -- incrementHistoryTree oldHistoryTreeModel history nextResponse = newHistoryTreeModel
  incrementHistoryTree :: historyTreeType -> [symbolType] -> symbolType -> historyTreeType
  incrementHistoryTree historyTree history nextResponse =
    increaseHistoryTree historyTree history nextResponse 1

  -- |Wrapper for increaseHistoryTree that keeps the model in a STRef.
  increaseHistoryTreeST ::
    STRef s historyTreeType -> [symbolType] -> symbolType -> Integer -> ST s ()

  -- |Wrapper for incrementHistoryTree that hides the model in a State Monad.
  incrementHistoryTreeST ::
    STRef s historyTreeType -> [symbolType] -> symbolType -> ST s ()
  incrementHistoryTreeST historyTreeRef history nextResponse =
    increaseHistoryTreeST historyTreeRef history nextResponse 1

  -- |Marks the indicated node of history tree data as a candidate for extensions.
  -- subsequent updates to the model can cause the creation of child nodes of this node.
  --
  -- The implementation is of the form:
  -- markExtensible oldHistoryTreeModel history = newHistoryTreeModel
  markExtensible :: historyTreeType -> [symbolType] -> historyTreeType

  -- |Wrapper for markExtensible that keeps the model in a STRef.
  markExtensibleST ::
    STRef s historyTreeType -> [symbolType] -> ST s ()

  maximumDepth :: historyTreeType -> Int

  -- |Returns the set of key combinations that point to updated distributions.
  updated :: historyTreeType -> Maybe UpdateMarkers

  -- |Clears the set of markers that indicate updated distributions.
  clearUpdated :: historyTreeType -> historyTreeType

-- EOF
