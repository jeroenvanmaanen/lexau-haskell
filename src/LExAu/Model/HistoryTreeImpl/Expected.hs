{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module LExAu.Model.HistoryTreeImpl.Expected
  ( UpdateStep
  , breakUp
  , createEmptyHistoryTreeExpected
  , createHistoryTreeExpected
  ) where

import Control.Monad (liftM)
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq, (|>))
import Text.Show (showListWith)
import qualified Data.Foldable as Fold (toList)
import qualified Data.IntMap as IntMap (alter, assocs, fromList, insert, keys, lookup, null)
import qualified Data.Sequence as Seq (empty, singleton)

import LExAu.API.Alphabet
  ( Alphabet
  , UpdatableAlphabet
  )
import LExAu.API.DescriptionLength
  ( DescriptionLength(descriptionLength)
  , PrefixEncode(prefixEncode)
  , toTerseCode
  )
import LExAu.API.Distribution
  ( BaseHistogram(addHistogram,intCount,maybeSubtractHistogram,subtractHistogram,weight)
  , Distribution
  , DistributionFactory(DistributionFactory, create)
  , DistributionType
  , Histogram
  , IndexedHistogram
  , IntDistribution(allInts,intProbability)
  , LogDistribution
  , NegatableHistogram
  , formatProbability
  , negateHistogram
  , straightHistogram
  )
import LExAu.API.HistoryTree
  ( UpdateMarkers(UpdateMarkers)
  )
import LExAu.API.Indexed (IndexedMember, index, member)
import LExAu.API.MDL
  ( Optimizer(optimize,updateOptimizerIO)
  , OptimizerType
  )
import LExAu.API.Model
  ( AlphabetUpdate(..)
  , HasCheckSum(checkSum)
  , ModelOfExpectedBehavior
  , OptimizeUpdate(..)
  , Updater(update)
  , UpdaterIO(updateIO)
  )
import LExAu.API.Named (Named(name))
import LExAu.Distribution.MDL (createMDLOptimizer,createMDLOptimizerFromCache)
import LExAu.Model.HistoryTreeImpl.Covered
  ( CoveredUpdate(CoveredUpdate)
  , phantomCovered
  , updateHistoryTreeCovered
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
  , maybeStatusChar
  , statusChar
  )
import LExAu.Model.HistoryTreeImpl.Observed
  ( HistoryTreeModelImpl(..)
  , createHistoryTreeModel
  )
import LExAu.Model.HistoryTreeImpl.Solid
  ( setSolid
  )
import LExAu.Model.HistoryTreeImpl.Utilities
  ( LogEntry(logEntry)
  )
import LExAu.Utilities.Data (flodder, flodderWithKey, tryToFind)
import LExAu.Utilities.DetailedException (enrich)
import LExAu.Utilities.Logging
  ( Loggable(logs)
  , Summarize(summarize)
  , logVerbose
  )

instance ModelOfExpectedBehavior (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) where

instance HasCheckSum (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) where
  checkSum = theDescriptionLength . theExtraProperties . theExpectedData

data ExpectedLogContext observedDistributionType expectedDistributionType = 
  ExpectedLogContext {
    theObservedNode :: HTData observedDistributionType (),
    theCoveredNode :: Maybe (HTData observedDistributionType ()),
    theExpectedNode :: HTData expectedDistributionType ExpectedExtraProperties
  }

logExpected :: (
      BaseHistogram observedDistributionType,
      IntDistribution expectedDistributionType,
      Alphabet alphabetType symbolType
    ) =>
  (ExpectedLogContext observedDistributionType expectedDistributionType) -> alphabetType -> String -> Int -> ShowS

logExpected context symbols indent key =
  let observedCount = intCount (theDistribution $ theObservedNode context) key
      coveredCount =
        case theCoveredNode context of
          Just coveredNode -> intCount (theDistribution coveredNode) key
          Nothing -> 0
      expectedProbability = intProbability (theDistribution $ theExpectedNode context) key
  in showString "\n" . 
     showString indent .
     shows (name (symbols `member` key)) .
     showString ": " .
     formatProbability expectedProbability .
     showString " (" .
     shows (observedCount - coveredCount) .
     showString " = " .
     shows observedCount .
     showString " - " .
     shows coveredCount .
     showString ")"

childLogContext :: (
        Show observedDistributionType,
        Show expectedDistributionType
      ) =>
    Int -> (ExpectedLogContext observedDistributionType expectedDistributionType) -> (ExpectedLogContext observedDistributionType expectedDistributionType)

childLogContext key context =
  let functionLabel = Seq.singleton "Child log context"
      childObservedNode = tryToFind (functionLabel |> "observed child") key (theChildren $ theObservedNode context)
      childCoveredNode =
        case theCoveredNode context of
          Just coveredNode -> IntMap.lookup key $ theChildren coveredNode
          Nothing -> Nothing
      childExpectedNode = tryToFind (functionLabel |> "expected child") key (theChildren $ theExpectedNode context)
  in ExpectedLogContext childObservedNode childCoveredNode childExpectedNode

instance (
        Alphabet alphabetType symbolType,
        BaseHistogram observedDistributionType,
        IntDistribution observedDistributionType,
        IntDistribution expectedDistributionType,
        DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
        PrefixEncode (Maybe [Int], DistributionType expectedDistributionType),
        LogDistribution alphabetType symbolType observedDistributionType,
        LogDistribution alphabetType symbolType expectedDistributionType
      ) =>
    LogEntry (ExpectedLogContext observedDistributionType expectedDistributionType) alphabetType symbolType where 
  logEntry symbols maybeUpdated indent (keyIndex, context) =
    let observedNode = theObservedNode context
        observedDistribution = theDistribution observedNode
        childNodes = theChildren observedNode
    in (showListWith (logExpected context symbols (indent ++ "     ")) $ allInts observedDistribution) .
       showString "\n  " .
       showString indent .
--       showString "<#" .
--       shows (
--         case theCoveredNode context of
--           Nothing -> 0
--           Just coveredNode -> IntMap.size $ theChildren coveredNode
--       ) .
--       showString "> " .
       (showListWith (logEntryWithKey symbols (indent ++ "   ")) $ map (\x -> (x, childLogContext x context)) (IntMap.keys childNodes))

logContextStatus :: (
        Alphabet alphabetType symbolType
     ) =>
   alphabetType -> (ExpectedLogContext observedDistributionType expectedDistributionType) -> ShowS

logContextStatus symbols context =
  let observedStatusChar = statusChar $ theObservedNode context
      coveredStatusChar = maybeStatusChar $ theCoveredNode context
      expectedNode = theExpectedNode context
      expectedStatusChar = statusChar expectedNode
      expectedDirty = if theDistributionDirty $ theExtraProperties expectedNode then '?' else '!'
  in showChar '-' .
     showChar '"' .
     showChar observedStatusChar .
     showChar coveredStatusChar .
     showChar expectedStatusChar .
     showChar expectedDirty .
     showChar '"'

logEntryWithKey :: (
        Alphabet alphabetType symbolType,
        BaseHistogram observedDistributionType,
        IntDistribution observedDistributionType,
        IntDistribution expectedDistributionType,
        DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
        PrefixEncode (Maybe [Int], DistributionType expectedDistributionType),
        LogDistribution alphabetType symbolType observedDistributionType,
        LogDistribution alphabetType symbolType expectedDistributionType
      ) =>
    alphabetType -> String -> (Int, ExpectedLogContext observedDistributionType expectedDistributionType) -> ShowS

logEntryWithKey symbols indent (key, context) =
  let observedStatusChar = statusChar $ theObservedNode context
      coveredStatusChar = maybeStatusChar $ theCoveredNode context
      expectedNode = theExpectedNode context
      expectedStatusChar = statusChar expectedNode
      parentKeyIndices = theParentKeyIndices $ theExtraProperties expectedNode
      prefixCode = prefixEncode ((), expectedNode)
      terseCode = toTerseCode $ prefixCode ""
      codeLength = length terseCode
  in showString "\n" .
     showString indent .
     shows (name (symbols `member` key)) .
     logContextStatus symbols context .
     showString ": [distribution length: " .
     shows (descriptionLength (parentKeyIndices, theDistribution expectedNode)) .
     showString "; expected node info: " .
     showString (theInfo $ theExtraProperties expectedNode) .
     showString "; subtree length: " .
     shows (nodeDescriptionLength expectedNode) .
     showString " == " .
     shows (descriptionLength ((), expectedNode)) .
     showString " == " .
     shows codeLength .
     showString "; " .
     prefixCode .
     showString "] " .
     logEntry symbols Nothing indent (key, context)

instance (
        Alphabet alphabetType symbolType,
        BaseHistogram observedDistributionType,
        IntDistribution observedDistributionType,
        IntDistribution expectedDistributionType,
        DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
        PrefixEncode (Maybe [Int], DistributionType expectedDistributionType),
        LogDistribution alphabetType symbolType observedDistributionType,
        LogDistribution alphabetType symbolType expectedDistributionType,
        Summarize (OptimizerType observedDistributionType expectedDistributionType oracleType)
      ) =>
    Loggable (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) where
  logs historyTreeExpected =
    let observedModel = theObservedModel historyTreeExpected
        symbols = theAlphabet observedModel
        expectedData = theExpectedData historyTreeExpected
        distribution = theDistribution expectedData
        childNodes = theChildren expectedData
        logContext = ExpectedLogContext (theData observedModel) (Just $ theCoveredData historyTreeExpected) expectedData
        prefixCode = prefixEncode ((), expectedData)
        terseCode = toTerseCode $ prefixCode ""
        codeLength = length terseCode
    in showString "{HistoryTreeExpected: " .
       summarize (theOptimizerState historyTreeExpected) .
       showString ";\n" .
       logContextStatus symbols logContext .
       showString ": distribution weight: " .
       shows (weight $ theDistribution $ theData observedModel) .
       showString ": [distribution length: " .
       shows (descriptionLength (Nothing :: Maybe [Int], theDistribution expectedData)) .
       showString "; expected node info: " .
       showString (theInfo $ theExtraProperties expectedData) .
       showString "; subtree length: " .
       shows (nodeDescriptionLength expectedData) .
       showString " == " .
       shows (descriptionLength ((), expectedData)) .
       showString " == " .
       shows codeLength .
       showString "; " .
       showString (chopString 50 terseCode) .
       showString "; " .
       prefixCode .
       showString "; " .
       logEntry symbols Nothing "" (0, logContext) .
       showString "]}"

chopString :: Int -> String -> String
chopString n [] = []
chopString n str =
  let start = take n str
      remainder = drop n str
  in case remainder of
       [] -> start
       _ -> start ++ " " ++ (chopString n remainder)

instance (
        Alphabet alphabetType symbolType,
        IntDistribution observedDistributionType,
        Distribution alphabetType symbolType observedDistributionType,
        BaseHistogram observedDistributionType,
        IndexedHistogram observedDistributionType,
        Histogram alphabetType symbolType observedDistributionType,
        DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
        IntDistribution expectedDistributionType,
        Eq expectedDistributionType,
        Updater CoveredUpdate (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) UpdateMarkers,
        Show observedDistributionType,
        Show expectedDistributionType,
        LogDistribution alphabetType symbolType observedDistributionType
      ) =>
    UpdaterIO OptimizeUpdate (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) (HistoryTreeModelImpl alphabetType symbolType observedDistributionType) where
  updateIO OptimizeUpdate updatedHistoryTreeModel oldHistoryTreeExpected =
    let oldHistoryTreeModel = theObservedModel oldHistoryTreeExpected
        modified = weight (theDistribution $ theData updatedHistoryTreeModel) /= weight (theDistribution $ theData oldHistoryTreeModel)
        maybeUpdatedCovered = updateHistoryTreeCovered updatedHistoryTreeModel oldHistoryTreeExpected
        updatedCovered = fromMaybe oldHistoryTreeExpected maybeUpdatedCovered
        updatedObserved = updatedCovered { theObservedModel = updatedHistoryTreeModel }
        updateExpected model expected =
          let updatedExpected = fromMaybe expected $ updateHistoryTreeExpected True updatedHistoryTreeModel expected
              paths = toPaths (theAlphabet model) (theUpdatedEntries model)
              (newHistoryTreeExpected, _) = flodder (\ path expected -> evaluateExpectedPath path expected) (updatedExpected, modified) paths
          in newHistoryTreeExpected
    in enrich
         ( showString "Modified and updated covered: "
         . shows (modified && isJust maybeUpdatedCovered)
         )
         ( if modified
           then
             do updatedOptimizer <- updateHistoryTreeOptimizer updatedHistoryTreeModel updatedObserved
                return $! Just $! updateExpected updatedHistoryTreeModel updatedOptimizer
           else
             return $ Nothing
         )

data UpdateStep alphabetType symbolType observedDistributionType
  = PrepareForPaths (HistoryTreeModelImpl alphabetType symbolType observedDistributionType)
  | UpdatePaths [[symbolType]]
  deriving Show

instance
    ( Show (HistoryTreeModelImpl alphabetType symbolType observedDistributionType)
    , Show symbolType
    ) =>
    Loggable (UpdateStep alphabetType symbolType observedDistributionType) where
  logs (PrepareForPaths model) = showString "PrepareForPaths " . shows model
  logs (UpdatePaths paths) = showString "UpdatePaths " . shows paths

breakUp
  :: (
    Alphabet alphabetType symbolType
  )
  => Int
  -> (HistoryTreeModelImpl alphabetType symbolType observedDistributionType)
  -> [UpdateStep alphabetType symbolType observedDistributionType]

breakUp chunkSize updatedObservedModel =
  let paths = toPaths (theAlphabet updatedObservedModel) (theUpdatedEntries updatedObservedModel)
  in ( PrepareForPaths updatedObservedModel
     : (map UpdatePaths $ splitChunks chunkSize paths)
     )

splitChunks :: Int -> [a] -> [[a]]
splitChunks _ [] = []
splitChunks n xs@(_ : _) =
  let (chunk, remainder) = splitAt n xs
  in (chunk : splitChunks n remainder)

instance (
        Alphabet alphabetType symbolType,
        IntDistribution observedDistributionType,
        Distribution alphabetType symbolType observedDistributionType,
        BaseHistogram observedDistributionType,
        IndexedHistogram observedDistributionType,
        Histogram alphabetType symbolType observedDistributionType,
        DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
        IntDistribution expectedDistributionType,
        Eq expectedDistributionType,
        Updater CoveredUpdate (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) UpdateMarkers,
        Show observedDistributionType,
        Show expectedDistributionType,
        LogDistribution alphabetType symbolType observedDistributionType
      ) =>
    UpdaterIO OptimizeUpdate (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) (UpdateStep alphabetType symbolType observedDistributionType) where
  updateIO OptimizeUpdate (PrepareForPaths updatedHistoryTreeModel) oldHistoryTreeExpected =
    let oldHistoryTreeModel = theObservedModel oldHistoryTreeExpected
        modified = weight (theDistribution $ theData updatedHistoryTreeModel) /= weight (theDistribution $ theData oldHistoryTreeModel)
        maybeUpdatedCovered = updateHistoryTreeCovered updatedHistoryTreeModel oldHistoryTreeExpected
        updatedCovered = fromMaybe oldHistoryTreeExpected maybeUpdatedCovered
        updatedObserved = updatedCovered { theObservedModel = updatedHistoryTreeModel }
        updateExpected expected = fromMaybe expected $ updateHistoryTreeExpected True updatedHistoryTreeModel expected
    in enrich
         ( showString "Modified and updated covered: "
         . shows (modified && isJust maybeUpdatedCovered)
         )
         ( if modified
           then
             do updatedOptimizer <- updateHistoryTreeOptimizer updatedHistoryTreeModel updatedObserved
                return $! Just $! updateExpected updatedOptimizer
           else
             return $ Nothing
         )
  updateIO OptimizeUpdate (UpdatePaths paths) oldHistoryTreeExpected =
    let historyTreeModel = theObservedModel oldHistoryTreeExpected
        updateExpected expected =
          let updatedExpected = fromMaybe expected $ updateHistoryTreeExpected True historyTreeModel expected
              (newHistoryTreeExpected, _) = flodder (\ path expected -> evaluateExpectedPath path expected) (updatedExpected, True) paths
          in newHistoryTreeExpected
    in return $! Just $! updateExpected oldHistoryTreeExpected

updateHistoryTreeOptimizer ::
      ( BaseHistogram observedDistributionType
      , IntDistribution observedDistributionType
      ) =>
    (HistoryTreeModelImpl alphabetType symbolType observedDistributionType)
      -> (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType)
      -> IO (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType)
updateHistoryTreeOptimizer updatedHistoryTreeModel updatedObserved =
  do let oldOptimizerState = theOptimizerState updatedObserved
         updatedData = theData updatedHistoryTreeModel
         biggestDistribution = theDistribution updatedData
         maxWeight = weight biggestDistribution
         distributionSize = toInteger $ length $ allInts biggestDistribution
         safeCeiling = maxWeight + distributionSize * distributionSize
     maybeNewOptimizerState <- updateOptimizerIO safeCeiling oldOptimizerState
     return $
       case maybeNewOptimizerState of
         Nothing -> updatedObserved
         Just newOptimizerState -> updatedObserved { theOptimizerState = newOptimizerState }

toPaths :: (
        Alphabet alphabetType symbolType
      ) =>
    alphabetType -> Maybe UpdateMarkers -> [[symbolType]]

toPaths alphabet (Just updateMarkers) =
  map Fold.toList $ toPathSeqs alphabet updateMarkers [Seq.empty]

toPaths alphabet Nothing = []

toPathSeqs :: (
        Alphabet alphabetType symbolType
      ) =>
    alphabetType -> UpdateMarkers -> [Seq symbolType] -> [Seq symbolType]

toPathSeqs alphabet (UpdateMarkers children) paths@(firstPath : _) =
  flodderWithKey (\ key childMarkers otherPaths -> toPathSeqs alphabet childMarkers ((firstPath |> (alphabet `member` key)) : otherPaths)) paths children

evaluateExpectedPath :: (
        Alphabet alphabetType symbolType,
        IndexedHistogram observedDistributionType,
        Histogram alphabetType symbolType observedDistributionType,
        DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
        IntDistribution expectedDistributionType,
        Eq expectedDistributionType,
        Updater CoveredUpdate (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) UpdateMarkers,
        Show observedDistributionType,
        Show expectedDistributionType,
        LogDistribution alphabetType symbolType observedDistributionType
      ) =>
    [symbolType] -> (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType, Bool) -> (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType, Bool)

evaluateExpectedPath [] result = result

evaluateExpectedPath path (expectedModel, oldUpdated) =
  let oldDescriptionLength = theDescriptionLength $ theExtraProperties $ theExpectedData expectedModel
      maybeResult = setSolid path toggleStatus expectedModel
      (alternativeDescriptionLength, alternativeExpectedModel, deltaDataDL) =
        case maybeResult of
          Just (otherExpectedModel, theDeltaDataDL) -> (theDescriptionLength $ theExtraProperties $ theExpectedData otherExpectedModel, otherExpectedModel, theDeltaDataDL)
          Nothing -> (oldDescriptionLength, expectedModel, 0)
      (newExpectedModel, newUpdated) =
        ( if alternativeDescriptionLength + (round deltaDataDL) < oldDescriptionLength
            then (alternativeExpectedModel, True)
            else (expectedModel, oldUpdated)
        )
  in enrich
       ( showListWith (showString . name) path
       . showString ": Covered data: "
       . logEntry (theAlphabet $ theObservedModel expectedModel) Nothing "" (-1, theCoveredData expectedModel)
       . showString ": Observed data: "
       . logEntry (theAlphabet $ theObservedModel expectedModel) Nothing "" (-1, theData $ theObservedModel expectedModel)
       )
       (seq (checkSum newExpectedModel) (newExpectedModel, newUpdated))

toggleStatus :: HTStatus -> HTStatus
toggleStatus Solid = Virtual
toggleStatus _ = Solid

instance (
        Distribution alphabetType symbolType observedDistributionType,
        BaseHistogram observedDistributionType,
        IntDistribution observedDistributionType,
        Eq expectedDistributionType,
        DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
        IntDistribution expectedDistributionType,
        Show observedDistributionType
      ) =>
    Updater CoveredUpdate (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) UpdateMarkers where
  update CoveredUpdate updateMarkers oldHistoryTreeExpected =
    let observedModel = theObservedModel oldHistoryTreeExpected
        updatedModel = observedModel { theUpdatedEntries = Just updateMarkers }
    in updateHistoryTreeExpected False updatedModel oldHistoryTreeExpected

instance (
      Alphabet alphabetType symbolType,
      UpdatableAlphabet alphabetType
    ) =>
    Updater AlphabetUpdate (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) alphabetType where
  update AlphabetUpdate newAlphabet oldExpected = liftM (\m -> oldExpected { theObservedModel = m }) $ update AlphabetUpdate newAlphabet (theObservedModel oldExpected)

createHistoryTreeExpectedInternal :: (
        DescriptionLength (Maybe [Int], DistributionType expectedDistributionType)
      ) =>
    (HistoryTreeModelImpl alphabetType symbolType observedDistributionType)
      -> (DistributionFactory observedDistributionType -> OptimizerType observedDistributionType expectedDistributionType oracleType)
      -> (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType)

createHistoryTreeExpectedInternal observed optimizerFactory =
  let distributionFactory = theFactory observed
      observedDistribution = theDistribution $ theData observed
      Just factory = create distributionFactory
      coveredDistribution = factory []
      coveredData = HTData coveredDistribution (IntMap.fromList []) Solid ()
      optimizerState = optimizerFactory distributionFactory
      expectedDistribution = optimize optimizerState observedDistribution
      tmpExpectedExtraProperties = ExpectedExtraProperties 0 0 True "tmp" Nothing
      expectedData = HTData expectedDistribution (IntMap.fromList []) Solid tmpExpectedExtraProperties
      expectedExtraProperties = ExpectedExtraProperties (descriptionLength ((), expectedData)) 0 True "create" Nothing
  in HistoryTreeExpectedImpl observed coveredData (expectedData { theExtraProperties = expectedExtraProperties }) optimizerState

createHistoryTreeExpected observed maybeCacheLocation =
  case maybeCacheLocation of
    Nothing -> createHistoryTreeExpectedInternal observed createMDLOptimizer
    Just cacheLocation -> createHistoryTreeExpectedInternal observed $ createMDLOptimizerFromCache cacheLocation

createEmptyHistoryTreeExpected alphabet factory maybeCacheLocation =
  let distribution =
        case factory of
          DistributionFactory (Just factoryMethod) -> factoryMethod []
      model = createHistoryTreeModel factory alphabet distribution
  in createHistoryTreeExpected model maybeCacheLocation

class DescriptionContext contextType distributionType extraPropertiesType where
  subContexts :: contextType -> HTData distributionType extraPropertiesType -> [(Int, (contextType, HTData distributionType extraPropertiesType))]
  contextStatus :: contextType -> HTData distributionType extraPropertiesType -> HTStatus
  addContextChildDescriptionLength :: (
        DescriptionLength (contextType, HTData distributionType extraPropertiesType)
      ) =>
    (Int, (contextType, HTData distributionType extraPropertiesType)) -> (Integer, Integer) -> (Integer, Integer)
  addContextChildDescriptionLength (key, (context, child)) (accumulator, count) =
    let childLength = descriptionLength (context, child)
    in if childLength <= 1
         then (accumulator, count)
         else (sum [
                  accumulator,
                  descriptionLength $ toInteger key,
                  childLength,
                  -1 -- Don't count the flag that indicates that the node is not empty
                ],
                count + 1)
  addContextChildCode :: (
        PrefixEncode (contextType, HTData distributionType extraPropertiesType)
      ) =>
    (Int, (contextType, HTData distributionType extraPropertiesType)) -> (ShowS, Integer) -> (ShowS, Integer)
  addContextChildCode (key, (context, child)) (buffer, count) =
    let childCode = prefixEncode (context, child)
        childCodeString = childCode ""
    in if (take 8 childCodeString) == "O(empty)"
         then (buffer, count)
         else (
             buffer .
               showString "child " .
               prefixEncode key .
               (showString $ drop (length "I(node)") childCodeString) .
               showString ", ",
             count + 1)

instance (
        DescriptionContext contextType distributionType ExpectedExtraProperties,
        DescriptionLength (Maybe [Int], DistributionType distributionType)
      ) =>
    DescriptionLength (contextType, HTData distributionType ExpectedExtraProperties) where
  descriptionLength (context, node) =
    let childrenAssocs = subContexts context node
        (childrenLength, childrenCount) = flodder addContextChildDescriptionLength (0, 0) $ childrenAssocs
        solidFlag = (contextStatus context node) == Solid
        contextLocalLength _ node =
            let parentKeyIndices = theParentKeyIndices $ theExtraProperties node
            in descriptionLength (parentKeyIndices, theDistribution node)
    in if solidFlag || (childrenLength > 0)
         then
           let localLength = if solidFlag then contextLocalLength context node else 0
           in sum
                [ 1 -- Flag to indicate that this node is not empty
                , 1 -- Flag to indicate the presence or absence of a local distribution
                , localLength
                , descriptionLength childrenCount
                , childrenLength
                ]
         else 1 -- Flag to indicate that this node is empty

instance (
        DescriptionContext contextType distributionType ExpectedExtraProperties,
        PrefixEncode (Maybe [Int], DistributionType distributionType)
      ) =>
    PrefixEncode (contextType, HTData distributionType ExpectedExtraProperties) where
  prefixEncode (_, node) =
    let childrenMapping = theChildren node
        (childrenCode, childrenCount) = flodder addContextChildCode (id, 0) $ subContexts () node
    in if ((theStatus node) == Solid) || (childrenCount > 0)
         then
           let parentKeyIndices = theParentKeyIndices $ theExtraProperties node
               localCode =
                 case theStatus node of
                   Solid ->
                     showString "I(solid)" .
                     (prefixEncode $ (parentKeyIndices, theDistribution node))
                   _ -> showString " O(virtual)"
           in showString "I(node){" .
              localCode .
              showString " - #" .
              prefixEncode childrenCount .
              showString ": " .
              childrenCode .
              showChar '}'
         else showString "O(empty)"

addContext :: contextType -> [(Int,nodeType)] -> [(Int,(contextType, nodeType))]
addContext context nodes = map (\(k,v) -> (k, (context, v))) nodes

-- Functions concerning the description (length) of HTData nodes irrespective of their parent
instance DescriptionContext () distributionType ExpectedExtraProperties where
  subContexts _ node = addContext () $ IntMap.assocs $ theChildren node
  contextStatus context node = theStatus node

instance (
        DescriptionLength (Maybe [Int], DistributionType distributionType)
      ) =>
    DescriptionLength (HTData distributionType ExpectedExtraProperties) where
  descriptionLength node = descriptionLength ((), node)

instance (
        PrefixEncode (Maybe [Int], DistributionType distributionType)
      ) =>
    PrefixEncode (HTData distributionType ExpectedExtraProperties) where
  prefixEncode node = prefixEncode ((), node)

-- Functions concerning the description (length) of HTData nodes in the context of their parent

-- TODO

data TreeOptimizerContext observedDistributionType expectedDistributionType =
  TreeOptimizerContext {
    theDirtyFlag :: Bool,
    theDescendantCoveredData :: Maybe (HTData observedDistributionType ()),
    theDescendantExpectedData :: HTData expectedDistributionType ExpectedExtraProperties,
    theAncestorExpectedDistribution :: DistributionType expectedDistributionType,
    thePathLength :: Int
  }

updateHistoryTreeExpected :: (
      Eq expectedDistributionType,
      DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
      IntDistribution expectedDistributionType,
      BaseHistogram observedDistributionType,
      IntDistribution observedDistributionType,
      Optimizer (OptimizerType observedDistributionType expectedDistributionType oracleType) observedDistributionType expectedDistributionType,
      Show observedDistributionType
    ) =>
  Bool -> (HistoryTreeModelImpl alphabetType symbolType observedDistributionType) -> (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) -> Maybe (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType)

updateHistoryTreeExpected force updatedModel oldExpected =
  case theUpdatedEntries updatedModel of
    Nothing -> Nothing
    Just (UpdateMarkers updateMarkers) ->
      let optimizerState = theOptimizerState oldExpected
          updatedData = theData updatedModel
          coveredData = theCoveredData oldExpected
          coveredDistribution = theDistribution coveredData
          effectiveDistribution = enrich (showString "Update history tree expected") $ subtractHistogram (theDistribution updatedData) coveredDistribution
          oldExpectedData = theExpectedData oldExpected
          (startDirtyFlag, startExpectedData) =
            updateHistoryTreeSingleExpectedDistribution force optimizerState Nothing effectiveDistribution (Just oldExpectedData)
          startTreeOptimizerContext =
            TreeOptimizerContext
              startDirtyFlag
              (Just coveredData)
              startExpectedData
              (theDistribution startExpectedData)
              0
          nextTreeOptimizerContext =
            flodderWithKey
              (updateHistoryTreeSingleExpected force optimizerState updatedData)
              startTreeOptimizerContext
              updateMarkers
          dirtyFlag = theDirtyFlag nextTreeOptimizerContext
          nextExpectedData = theDescendantExpectedData nextTreeOptimizerContext
          newObservedModel = updatedModel { theUpdatedEntries = Nothing }
          nextExpected = oldExpected { theObservedModel = newObservedModel, theExpectedData = nextExpectedData }
      in seq
           (theDescriptionLength $ theExtraProperties startExpectedData)
           ( if dirtyFlag
               then Just $ nextExpected
               else Nothing
           )

updateHistoryTreeSingleExpectedDistribution :: (
      Eq expectedDistributionType,
      DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
      BaseHistogram observedDistributionType,
      IntDistribution observedDistributionType
    ) =>
  Bool -> (OptimizerType observedDistributionType expectedDistributionType oracleType) -> Maybe [Int] -> (DistributionType observedDistributionType) -> (Maybe (HTData expectedDistributionType ExpectedExtraProperties)) -> (Bool, (HTData expectedDistributionType ExpectedExtraProperties))

updateHistoryTreeSingleExpectedDistribution force optimizerState maybeParentKeyIndices effectiveDistribution maybeOldExpectedData =
  let newExpectedDistribution = optimize optimizerState effectiveDistribution
  in case maybeOldExpectedData of
       Just oldExpectedData ->
         let oldExpectedDistribution = theDistribution oldExpectedData
             oldExtraProperties = theExtraProperties oldExpectedData
             oldDistributionDirty = force || theDistributionDirty oldExtraProperties
             dirty = newExpectedDistribution /= oldExpectedDistribution
             newExpectedDataObject =
               if oldDistributionDirty && dirty
                 then
                   let newExtraProperties =
                         if (theStatus oldExpectedData) == Solid
                           then
                             let oldDistributionLength = descriptionLength ((theParentKeyIndices oldExtraProperties), oldExpectedDistribution)
                                 oldSubtreeLength = theDescriptionLength oldExtraProperties
                                 oldInfo = theInfo oldExtraProperties
                                 newDistributionLength = descriptionLength (maybeParentKeyIndices, newExpectedDistribution)
                                 newSubtreeLength = oldSubtreeLength + newDistributionLength - oldDistributionLength
                             in addInfo (oldExtraProperties { theDescriptionLength = newSubtreeLength }) (
                                    showString "updated-distribution: " .
                                    shows oldSubtreeLength .
                                    showString " + " .
                                    shows newDistributionLength .
                                    showString " - " .
                                    shows oldDistributionLength .
                                    showString " = " .
                                    shows newSubtreeLength
                                  )
                           else oldExtraProperties
                   in oldExpectedData { theDistribution = newExpectedDistribution, theExtraProperties = newExtraProperties { theParentKeyIndices = maybeParentKeyIndices, theDistributionDirty = False } }
                 else
                   if oldDistributionDirty
                     then oldExpectedData { theExtraProperties = oldExtraProperties { theParentKeyIndices = maybeParentKeyIndices, theDistributionDirty = False } }
                     else oldExpectedData
         in (dirty || oldDistributionDirty, newExpectedDataObject)
       Nothing ->
         let newExpectedData = HTData newExpectedDistribution (IntMap.fromList []) Virtual (ExpectedExtraProperties 1 0 False "update-empty" maybeParentKeyIndices)
         in (True, newExpectedData)

updateHistoryTreeSingleExpected :: (
      Eq expectedDistributionType,
      DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
      IntDistribution expectedDistributionType,
      BaseHistogram observedDistributionType,
      IntDistribution observedDistributionType,
      Show observedDistributionType
    ) =>
  Bool -> (OptimizerType observedDistributionType expectedDistributionType oracleType) -> (HTData observedDistributionType ()) -> Int -> UpdateMarkers -> TreeOptimizerContext observedDistributionType expectedDistributionType -> TreeOptimizerContext observedDistributionType expectedDistributionType

updateHistoryTreeSingleExpected force optimizerState updatedData key childUpdateMarkers oldTreeOptimizerContext =
  let functionLabel = Seq.singleton "Update history-tree single expected"
      oldExpectedData = theDescendantExpectedData oldTreeOptimizerContext
      updatedChildData = tryToFind (functionLabel |> "updated child data") key (theChildren updatedData)
      updatedChildDistribution = theDistribution updatedChildData
      maybeOldCoveredChildData =
        do descendantCoveredData <- theDescendantCoveredData oldTreeOptimizerContext
           key `IntMap.lookup` (theChildren descendantCoveredData)
      effectiveChildDistribution =
        case maybeOldCoveredChildData of
          Just oldCoveredChildData -> enrich (showString "Update history tree single expected: ") $ subtractHistogram updatedChildDistribution $ theDistribution oldCoveredChildData
          Nothing -> updatedChildDistribution
      oldChildren = theChildren oldExpectedData
      maybeOldExpectedChildData = key `IntMap.lookup` oldChildren
      ancestorTreeOptimizerContext = 
        if theStatus oldExpectedData == Solid
          then oldTreeOptimizerContext { theAncestorExpectedDistribution = theDistribution oldExpectedData }
          else oldTreeOptimizerContext
      parentKeyIndices = sort $ allInts $ theAncestorExpectedDistribution ancestorTreeOptimizerContext
      (firstDirtyFlag, startExpectedChildData) = updateHistoryTreeSingleExpectedDistribution force optimizerState (Just parentKeyIndices) effectiveChildDistribution maybeOldExpectedChildData

      startDirtyFlag = (theDirtyFlag oldTreeOptimizerContext) || firstDirtyFlag
      oldChildDescriptionLength =
        case maybeOldExpectedChildData of
          Just oldExpectedChildData -> nodeDescriptionLength oldExpectedChildData
          Nothing -> 1
      startTreeOptimizerContext = ancestorTreeOptimizerContext { theDirtyFlag = startDirtyFlag, theDescendantCoveredData = maybeOldCoveredChildData, theDescendantExpectedData = startExpectedChildData }
      UpdateMarkers childUpdateMarkersMap = childUpdateMarkers
      nextTreeOptimizerContext = flodderWithKey (updateHistoryTreeSingleExpected force optimizerState updatedChildData) startTreeOptimizerContext childUpdateMarkersMap
      newDirtyFlag = theDirtyFlag nextTreeOptimizerContext

      newExpectedData =
        if newDirtyFlag
          then
            let newExpectedChildData = theDescendantExpectedData nextTreeOptimizerContext
                newChildren = IntMap.insert key newExpectedChildData oldChildren
                newChildDescriptionLength = nodeDescriptionLength startExpectedChildData
                deltaChildDescriptionLength = newChildDescriptionLength - oldChildDescriptionLength
                oldExtraProperties = theExtraProperties oldExpectedData
                oldDescriptionLength = theDescriptionLength oldExtraProperties
                newDescriptionLength = oldDescriptionLength + deltaChildDescriptionLength
                newExtraProperties = addInfo (oldExtraProperties { theDescriptionLength = newDescriptionLength }) (
                    showString "updated: " .
                    shows oldDescriptionLength .
                    showString " + " .
                    shows newChildDescriptionLength .
                    showString " - " .
                    shows oldChildDescriptionLength
                  )
            in if deltaChildDescriptionLength == 0
                 then oldExpectedData { theChildren = newChildren }
                 else oldExpectedData { theChildren = newChildren, theExtraProperties = newExtraProperties }
          else oldExpectedData
  in seq (theDescriptionLength $ theExtraProperties newExpectedData) $ oldTreeOptimizerContext { theDirtyFlag = newDirtyFlag, theDescendantExpectedData = newExpectedData }

--EOF
