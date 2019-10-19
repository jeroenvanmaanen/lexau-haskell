{-# LANGUAGE FlexibleContexts #-}
module LExAu.Model.HistoryTreeImpl.Solid
  ( setSolid
  ) where

import Control.Monad (liftM)
import Data.Sequence ((|>))
import Text.Show (showListWith)
import qualified Data.IntMap as IntMap (alter, fromList, insert, lookup, null)
import qualified Data.Sequence as Seq (singleton)

import LExAu.API.Alphabet
  ( Alphabet
  )
import LExAu.API.DescriptionLength
  ( DescriptionLength(descriptionLength)
  , PrefixEncode
  )
import LExAu.API.Distribution
  ( BaseHistogram
      ( addHistogram
      , intCount
      , maybeSubtractHistogram
      , subtractHistogram
      , weight
      )
  , DistributionFactory(DistributionFactory)
  , DistributionType
  , Histogram
  , IndexedHistogram
  , IntDistribution(allInts, intProbability)
  , LogDistribution
  , NegatableHistogram
  , negateHistogram
  , straightHistogram
  )
import LExAu.API.HistoryTree
  ( UpdateMarkers(UpdateMarkers)
  )
import LExAu.API.Indexed
  ( IndexedMember
  , index
  )
import LExAu.API.Model
  ( Updater(update)
  )
import LExAu.API.Named (Named(name))
import LExAu.Distribution.MDL (createMDLOptimizer)
import LExAu.Model.HistoryTreeImpl.Covered
  ( CoveredUpdate(CoveredUpdate)
  , phantomCovered
  )
import LExAu.Model.HistoryTreeImpl.ExpectedData
  ( ExpectedExtraProperties(..)
  , HistoryTreeExpectedImpl(..)
  , addInfo
  , nodeDescriptionLength
  )
import LExAu.Model.HistoryTreeImpl.HTData
  ( HTData(..)
  , HTStatus(..)
  , statusChar
  )
import LExAu.Model.HistoryTreeImpl.Observed
  ( HistoryTreeModelImpl(..)
  )
import LExAu.Utilities.Data (flodder, tryToFind)
import LExAu.Utilities.DetailedException (enrich)

data DirtySetSolidResult distributionType expectedDistributionType =
  DirtySetSolidResult
    { dssrMaybeNewCoveredNode :: Maybe (HTData distributionType ())
    , dssrNewExpectedNode :: (HTData expectedDistributionType ExpectedExtraProperties)
    , dssrNewDataDL :: Double
    , dssrDifference :: DistributionType (NegatableHistogram distributionType)
    , dssrDirtyFlag :: Bool
    }

-- |Returns a copy of the given covered and expected data structures where the
-- status of the node indicated by the list of symbols is set to Solid.
-- For the covered data structure the histograms are adjusted as well. For the
-- expected data structure only the status of the indicated node is changed.
dirtySetSolid :: (
      DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
      IntDistribution expectedDistributionType,
      IndexedHistogram distributionType,
      IndexedMember symbolType Int,
      Named symbolType,
      Show expectedDistributionType,
      Show distributionType
    ) =>
  DistributionType distributionType
    -> [symbolType]
    -> [symbolType]
    -> (HTStatus -> HTStatus)
    -> (HTData distributionType ())
    -> (Maybe (HTData distributionType ()))
    -> (HTData expectedDistributionType ExpectedExtraProperties)
    -> (DirtySetSolidResult distributionType expectedDistributionType)

dirtySetSolid emptyHistogram crumbs [] statusOperation oldObservedData maybeOldCoveredData oldExpectedData =
  let oldCoveredStatus =
        case maybeOldCoveredData of
          Nothing -> Virtual
          Just oldCoveredData -> theStatus oldCoveredData
      status = statusOperation oldCoveredStatus
      oldExpectedStatus = theStatus oldExpectedData
      oldExtraProperties = theExtraProperties oldExpectedData
      oldExpectedDescriptionLength = theDescriptionLength oldExtraProperties
      oldDataDL =
        if oldExpectedStatus == Solid
          then dataDescriptionLength (theDistribution oldObservedData) (liftM theDistribution maybeOldCoveredData) (theDistribution oldExpectedData)
          else 0
      emptyDifference = straightHistogram emptyHistogram
  in if oldCoveredStatus /= oldExpectedStatus
       then error $ "Inconsistent status: " ++ (show oldCoveredStatus) ++ ": " ++ (show oldExpectedStatus)
       else
         if oldExpectedStatus == status
           then DirtySetSolidResult maybeOldCoveredData oldExpectedData oldDataDL emptyDifference False
           else
             let oldObservedDistribution = theDistribution oldObservedData
                 parentKeyIndices = theParentKeyIndices oldExtraProperties
             in case status of
                  Solid ->
                    let (newCoveredData, difference) =
                          case maybeOldCoveredData of
                            Nothing -> (HTData emptyHistogram (IntMap.fromList []) status (), straightHistogram oldObservedDistribution)
                            Just oldCoveredData -> (oldCoveredData { theStatus = status }, enrich (showString "dirtySetSolid/Solid" . showListWith (showString . name) (reverse crumbs)) $ straightHistogram $ subtractHistogram oldObservedDistribution $ theDistribution oldCoveredData)
                        solidChildrenCount = theSolidChildrenCount oldExtraProperties
                        oldTrivial = solidChildrenCount < 1
                        deltaNodeDescriptionLength =
                          if oldTrivial
                            then 2 -- One bit for the Solid/Virtual flag and one bit to encode #solid children == 0
                            else 0
                        newExpectedDescriptionLength = oldExpectedDescriptionLength + (descriptionLength (parentKeyIndices, theDistribution oldExpectedData)) + deltaNodeDescriptionLength
                        newExtraProperties = oldExtraProperties { theDescriptionLength = newExpectedDescriptionLength, theInfo = "new-solid" }
                    in DirtySetSolidResult (Just newCoveredData) (oldExpectedData { theStatus = status, theExtraProperties = newExtraProperties }) oldDataDL difference True
                  Virtual ->
                    case maybeOldCoveredData of
                      Just oldCoveredData -> 
                        let startCounts = theDistribution oldCoveredData
                            difference = enrich (showString "dirtySetSolid/Virtual") $ negateHistogram $ subtractHistogram oldObservedDistribution startCounts
                            newExpectedDescriptionLength =
                              if IntMap.null $ theChildren oldExpectedData
                                then 1
                                else oldExpectedDescriptionLength - (descriptionLength (parentKeyIndices, theDistribution oldExpectedData))
                            newExtraProperties = addInfo (oldExtraProperties { theDescriptionLength = newExpectedDescriptionLength }) (
                                showString "new-virtual: " .
                                shows (weight difference)
                              )
                        in DirtySetSolidResult (phantomCovered $ oldCoveredData { theStatus = status }) (oldExpectedData { theStatus = status, theExtraProperties = newExtraProperties }) oldDataDL difference True

dirtySetSolid emptyHistogram crumbs (symbol : symbols) statusOperation oldObservedData maybeOldCoveredData oldExpectedData =
  let functionLabel = Seq.singleton "Dirty set solid"
      key = index symbol
      childOldObservedData = tryToFind (functionLabel |> "child old observed data") key (theChildren oldObservedData)
      oldCoveredChildren =
        case maybeOldCoveredData of
          Nothing -> IntMap.fromList []
          Just oldCoveredData -> theChildren oldCoveredData
      oldExtraProperties = theExtraProperties oldExpectedData
      oldDescriptionLength = theDescriptionLength oldExtraProperties
      maybeChildOldCoveredData = key `IntMap.lookup` oldCoveredChildren
      childOldExpectedData = tryToFind (functionLabel |> "child old expected data") key (theChildren oldExpectedData)
      childCrumbs = symbol : crumbs
      DirtySetSolidResult maybeChildNewCoveredData childNewExpectedData childOldDataDL childDifference dirty = dirtySetSolid emptyHistogram childCrumbs symbols statusOperation childOldObservedData maybeChildOldCoveredData childOldExpectedData
      newCoveredChildren = IntMap.alter (\x -> maybeChildNewCoveredData) key oldCoveredChildren
      newExpectedChildren = IntMap.insert key childNewExpectedData (theChildren oldExpectedData)
      (oldSolidChildrenContribution, oldChildDescriptionLength) = childContribution key childOldExpectedData
      (newSolidChildrenContribution, newChildDescriptionLength) = childContribution key childNewExpectedData
      deltaSolidChildrenCount = newSolidChildrenContribution - oldSolidChildrenContribution
      oldSolidChildrenCount = theSolidChildrenCount oldExtraProperties
      newSolidChildrenCount = oldSolidChildrenCount + deltaSolidChildrenCount
      oldTrivial = (oldSolidChildrenCount < 1) && ((theStatus oldExpectedData) /= Solid)
      newTrivial = (newSolidChildrenCount < 1) && ((theStatus oldExpectedData) /= Solid)
      deltaCountDescriptionLength = (countDescriptionLength newTrivial newSolidChildrenCount) - (countDescriptionLength oldTrivial oldSolidChildrenCount)
      deltaStatusDescriptionLength = if oldTrivial then 1 else 0
      (newDescriptionLength, info) =
        if newTrivial
          then (1, showString "trivial")
          else (
              oldDescriptionLength + newChildDescriptionLength - oldChildDescriptionLength + deltaCountDescriptionLength + deltaStatusDescriptionLength,
              shows oldDescriptionLength .
                showString " + " .
                shows newChildDescriptionLength .
                showString " - " .
                shows oldChildDescriptionLength .
                showString " + " .
                shows deltaCountDescriptionLength .
                showString " + " .
                shows deltaStatusDescriptionLength .
                showString " | " .
                shows (descriptionLength $ toInteger key) .
                showString ", '" .
                showChar (statusChar childNewExpectedData) .
                showString "', #" .
                shows (theSolidChildrenCount $ theExtraProperties childNewExpectedData)
            )
  in if dirty
       then
         if weight childDifference == 0
           then
             let newCoveredData =
                   case maybeOldCoveredData of
                     Nothing -> HTData (addHistogram emptyHistogram childDifference) newCoveredChildren Virtual ()
                     Just oldCoveredData -> oldCoveredData { theChildren = newCoveredChildren }
                 newExtraProperties = addInfo (oldExtraProperties { theDescriptionLength = newDescriptionLength, theSolidChildrenCount = newSolidChildrenCount, theDistributionDirty = True }) (showString "updated-children: " . info)
             in DirtySetSolidResult (Just newCoveredData) (oldExpectedData { theChildren = newExpectedChildren, theExtraProperties = newExtraProperties }) childOldDataDL childDifference dirty
           else
             let oldObservedDistribution = theDistribution oldObservedData
                 oldExpectedDistribution = theDistribution oldExpectedData
                 (maybeNewCoveredData, newExtraProperties, difference) =
                   if newTrivial
                     then
                       (Nothing, oldExtraProperties { theDescriptionLength = 1, theSolidChildrenCount = 0, theInfo = "removed-parent" }, childDifference)
                     else
                       case maybeOldCoveredData of
                         Nothing ->
                           let newDescriptionLength = 2 + (descriptionLength $ toInteger newSolidChildrenCount) + newChildDescriptionLength
                               newExtraProperties = addInfo (oldExtraProperties { theDescriptionLength = newDescriptionLength, theSolidChildrenCount = newSolidChildrenCount, theDistributionDirty = True }) (
                                   showString "inserted-virtual-parent: " .
                                   shows 2 .
                                   showString " + " .
                                   shows (descriptionLength $ toInteger newSolidChildrenCount) .
                                   showString " + " .
                                   shows newChildDescriptionLength .
                                   showString " | " .
                                   shows (nodeDescriptionLength childNewExpectedData)
                                 )
                               newCoveredData = HTData (addHistogram emptyHistogram childDifference) newCoveredChildren Virtual ()
                           in (Just newCoveredData, newExtraProperties, childDifference)
                         Just oldCoveredData ->
                           let parentDifference =
                                 case theStatus oldCoveredData of
                                   Solid -> straightHistogram emptyHistogram
                                   Virtual -> childDifference
                               newCoveredDistribution =
                                 enrich (
                                     showString "Dirty set solid: ancestor: Just -> Non-trivial: " .
                                     shows (theDistribution oldCoveredData) .
                                     showString ": " .
                                     shows childDifference .
                                     showString ": " .
                                     showListWith (showString . name) (reverse crumbs)
                                   ) $ addHistogram (theDistribution oldCoveredData) childDifference
                               newExtraProperties = addInfo (oldExtraProperties { theDescriptionLength = newDescriptionLength, theSolidChildrenCount = newSolidChildrenCount }) (showString "updated-parent: " . info)
                           in (Just $ oldCoveredData { theDistribution = newCoveredDistribution, theChildren = newCoveredChildren }, newExtraProperties { theDistributionDirty = True } , parentDifference)
                 oldDataDL = childOldDataDL +
                   if (theStatus oldExpectedData) == Solid
                     then
                       dataDescriptionLength (theDistribution oldObservedData) (liftM theDistribution maybeOldCoveredData) (theDistribution oldExpectedData)
                     else 0
             in DirtySetSolidResult maybeNewCoveredData (oldExpectedData { theChildren = newExpectedChildren, theExtraProperties = newExtraProperties }) oldDataDL difference dirty
       else DirtySetSolidResult maybeOldCoveredData oldExpectedData childOldDataDL childDifference dirty

childContribution :: Int -> HTData expectedDistributionType ExpectedExtraProperties -> (Int, Integer)
childContribution key expectedData =
  let extraProperties = theExtraProperties expectedData
      nodeDescriptionLength = theDescriptionLength extraProperties
      solidChildrenCount = theSolidChildrenCount extraProperties
      status = theStatus expectedData
      count = (status == Solid) || (solidChildrenCount > 0)
  in if count
       then (1, (descriptionLength $ toInteger key) + nodeDescriptionLength - 1)
       else (0, 0)

countDescriptionLength :: Bool -> Int -> Integer
countDescriptionLength trivial count =
  if trivial
    then 0
    else descriptionLength $ toInteger count

dataDescriptionLength :: (
        BaseHistogram distributionType,
        IntDistribution distributionType,
        IntDistribution expectedDistributionType
      ) =>
    DistributionType distributionType -> Maybe (DistributionType distributionType) -> DistributionType expectedDistributionType -> Double

dataDescriptionLength observedDistribution maybeCoveredDistribution expectedDistribution =
  let dataHistogram = maybeSubtractHistogram observedDistribution maybeCoveredDistribution
      addItem key previous =
        let count = intCount dataHistogram key
        in if count < 1
             then 0
             else
               let probability = intProbability expectedDistribution key
               in if probability == 0
                  then 0 -- error $ (showString "Zero probability for non-zero count: " . shows key . showString ": " . shows count) ""
                  else
                    let approximateProbability :: Double
                        approximateProbability = fromRational probability
                        averageBitLength = logBase 2.0 approximateProbability
                    in averageBitLength * fromIntegral count
  in flodder addItem 0 $ allInts dataHistogram

setSolid :: (
      Alphabet alphabetType symbolType,
      IndexedMember symbolType Int,
      IndexedHistogram observedDistributionType,
      DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
      IntDistribution expectedDistributionType,
      Histogram alphabetType symbolType observedDistributionType,
      Updater CoveredUpdate (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) UpdateMarkers,
      Eq expectedDistributionType,
      Show observedDistributionType,
      Show expectedDistributionType
    ) =>
  [symbolType] -> (HTStatus -> HTStatus) -> (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) -> Maybe ((HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType), Double)

setSolid path statusOperation oldExpected =
  let oldObservedModel = theObservedModel oldExpected
      oldObservedData = theData oldObservedModel
      oldCoveredData = theCoveredData oldExpected
      oldExpectedData = theExpectedData oldExpected
      emptyHistogram =
        case theFactory oldObservedModel of
          DistributionFactory (Just create) -> create []
          _ -> undefined
      DirtySetSolidResult maybeNewCoveredData newExpectedData oldDataDL _ dirty = dirtySetSolid emptyHistogram [] path statusOperation oldObservedData (Just oldCoveredData) oldExpectedData
  in if dirty
       then
         case maybeNewCoveredData of
           Just newCoveredData ->
             let dirtyNewExpected = oldExpected { theCoveredData = newCoveredData, theExpectedData = newExpectedData }
                 updatedEntries = toUpdatedEntries path
                 cleanNewExpected =
                   case update CoveredUpdate updatedEntries dirtyNewExpected of
                     Just clean -> clean
                     Nothing -> dirtyNewExpected
                 (newDataDL, _) = pathDataDescriptionLength path oldObservedData maybeNewCoveredData newExpectedData
             in Just (cleanNewExpected, newDataDL - oldDataDL)
           Nothing -> error "Cannot set the root node to virtual"
       else Nothing

toUpdatedEntries :: (IndexedMember symbolType Int) => [symbolType] -> UpdateMarkers

toUpdatedEntries [] = UpdateMarkers (IntMap.fromList [])

toUpdatedEntries (symbol : symbols) = UpdateMarkers (IntMap.fromList [(index symbol, toUpdatedEntries symbols)])

pathDataDescriptionLength :: (
        IndexedMember symbolType Int,
        BaseHistogram observedDistributionType,
        IntDistribution observedDistributionType,
        IntDistribution expectedDistributionType,
        Show observedDistributionType,
        Show expectedDistributionType
      ) =>
    [symbolType] -> HTData observedDistributionType () -> Maybe (HTData observedDistributionType ()) -> HTData expectedDistributionType ExpectedExtraProperties -> (Double, Bool)

pathDataDescriptionLength [] observedNode maybeCoveredNode expectedNode =
  if (theStatus expectedNode) == Solid
    then (dataDescriptionLength (theDistribution observedNode) (liftM theDistribution maybeCoveredNode) (theDistribution expectedNode), True)
    else (0, True)

pathDataDescriptionLength (symbol : symbols) observedNode maybeCoveredNode expectedNode =
  let functionLabel = Seq.singleton "Path data description length"
      key = index symbol
      childObservedNode = tryToFind (functionLabel |> "child observed node") key (theChildren observedNode)
      maybeChildCoveredNode = maybeCoveredNode >>= (IntMap.lookup key) . theChildren
      childExpectedNode = tryToFind (functionLabel |> "child expected node") key (theChildren expectedNode)
      (childDataDescriptionLength, childVisibleFlag) = pathDataDescriptionLength symbols childObservedNode maybeChildCoveredNode childExpectedNode
      result = childDataDescriptionLength +
        if childVisibleFlag
          then dataDescriptionLength (theDistribution observedNode) (liftM theDistribution maybeCoveredNode) (theDistribution expectedNode)
          else 0
      visibleFlag = childVisibleFlag && theStatus expectedNode /= Solid
   in (result, visibleFlag)
