
module LExAu.Model.HistoryTreeImpl.Covered
  ( CoveredUpdate(CoveredUpdate)
  , phantomCovered
  , updateHistoryTreeCovered
  ) where

import Data.Sequence ((|>))
import qualified Data.IntMap as IntMap (insert, keys, lookup, null)
import qualified Data.Sequence as Seq (singleton)

import LExAu.API.Distribution
  ( BaseHistogram(addHistogram, subtractHistogram, weight)
  , DistributionType
  , IndexedHistogram
  )
import LExAu.API.HistoryTree
  ( UpdateMarkers(UpdateMarkers)
  )
import LExAu.Model.HistoryTreeImpl.ExpectedData
  ( HistoryTreeExpectedImpl(..)
  )
import LExAu.Model.HistoryTreeImpl.HTData
  ( HTData(..)
  , HTStatus(..)
  )
import LExAu.Model.HistoryTreeImpl.Observed
  ( HistoryTreeModelImpl(HistoryTreeModelImpl,theData,theUpdatedEntries)
  )
import LExAu.Utilities.DetailedException (enrich)
import LExAu.Utilities.Data (flodder, tryToFind)

data CoveredUpdate = CoveredUpdate

data UpdateCoveredContext distributionType =
  UpdateCoveredContext {
    theOldData :: HTData distributionType (),
    theNewData :: HTData distributionType (),
    theUpdateMarkers :: UpdateMarkers,
    theCoveredContextData :: HTData distributionType (),
    theDifference :: Maybe (DistributionType distributionType),
    theCoveredDataUpdated :: Bool
  }

updateHistoryTreeCovered :: (
      IndexedHistogram observedDistributionType,
      Show observedDistributionType
    ) =>
  (HistoryTreeModelImpl alphabetType symbolType observedDistributionType) -> (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) -> Maybe (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType)

updateHistoryTreeCovered newModel oldExpected =
  case theUpdatedEntries newModel of
    Nothing -> Nothing
    Just updateMarkers ->
      let oldModel = theObservedModel oldExpected
          oldCoveredData = (theCoveredData oldExpected)
          context = UpdateCoveredContext (theData oldModel) (theData newModel) updateMarkers oldCoveredData Nothing False
          result = recursiveUpdateHistoryTreeCovered context
      in if theCoveredDataUpdated result
           then Just $ oldExpected { theObservedModel = newModel, theCoveredData = theCoveredContextData result }
           else Nothing

recursiveUpdateHistoryTreeCovered :: (
      IndexedHistogram distributionType,
      Show distributionType
    ) =>
  (UpdateCoveredContext distributionType) -> (UpdateCoveredContext distributionType)

recursiveUpdateHistoryTreeCovered oldContext =
  let UpdateMarkers updateMarkers = theUpdateMarkers oldContext
      oldCoveredData = theCoveredContextData oldContext
      oldDistribution = theDistribution oldCoveredData
      nextContext =
        if (theStatus oldCoveredData == Solid) && (weight oldDistribution) < 1
          then oldContext
          else
            let updatedKeys = IntMap.keys updateMarkers
            in flodder updateHistoryTreeCoveredForKey oldContext updatedKeys
  in case theStatus oldCoveredData of
       Solid ->
         let oldObservedDistribution = theDistribution $ theOldData oldContext
             newObservedDistribution = theDistribution $ theNewData oldContext
         in if (weight newObservedDistribution) == (weight oldObservedDistribution)
              then nextContext { theDifference = Nothing }
              else nextContext { theDifference = Just $ enrich (showString "Recursive update history tree covered") $ subtractHistogram newObservedDistribution oldObservedDistribution }
       Virtual -> nextContext

updateHistoryTreeCoveredForKey :: (
      IndexedHistogram distributionType,
      Show distributionType
    ) =>
  Int -> (UpdateCoveredContext distributionType) -> (UpdateCoveredContext distributionType)

updateHistoryTreeCoveredForKey key oldContext =
  let functionLabel = Seq.singleton "Update history-tree covered for key"
      oldCoveredData = theCoveredContextData oldContext
      oldChildren = theChildren oldCoveredData
      newContext =
        case IntMap.lookup key oldChildren of
          Nothing -> oldContext
          Just childCoveredData ->
            let childOldData = tryToFind (functionLabel |> "child old data") key (theChildren $ theOldData oldContext)
                childNewData = tryToFind (functionLabel |> "child new data") key (theChildren $ theNewData oldContext)
                UpdateMarkers updateMarkers = theUpdateMarkers oldContext
                childUpdateMarkers = tryToFind (functionLabel |> "update markers") key updateMarkers
                childContext = UpdateCoveredContext childOldData childNewData childUpdateMarkers childCoveredData Nothing False
                childResult = recursiveUpdateHistoryTreeCovered childContext
            in case theDifference childResult of
                 Nothing -> oldContext
                 Just childDifference ->
                   let oldDistribution = theDistribution oldCoveredData
                       newDistribution = enrich (showString "Update history tree covered for key" . shows key . showString "new covered distribution") $ addHistogram oldDistribution childDifference
                       newChildren = IntMap.insert key (theCoveredContextData childResult) oldChildren
                       newCoveredData = oldCoveredData { theDistribution = newDistribution, theChildren = newChildren }
                       maybeOldDifference = theDifference oldContext
                       newDifference =
                         case maybeOldDifference of
                           Nothing -> childDifference
                           Just oldDifference -> enrich (showString "Update history tree covered for key" . shows key . showString "new difference") $ addHistogram oldDifference childDifference
                   in oldContext { theCoveredContextData = newCoveredData, theDifference = Just newDifference, theCoveredDataUpdated = True }
  in seq (weight $ theDistribution $ theCoveredContextData newContext) newContext

phantomCovered :: (BaseHistogram distributionType) => (HTData distributionType extraPropertiesType) -> (Maybe (HTData distributionType extraPropertiesType))

phantomCovered covered =
  let distribution = theDistribution covered
      status = theStatus covered
      children = theChildren covered
  in if status == Virtual && (weight distribution) < 1 && (IntMap.null children)
       then Nothing
       else Just covered
