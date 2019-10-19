{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}
-- |Markov implementation of a Model.
module LExAu.Model.Markov (
    createEmptyMarkovExpected,
    createEmptyMarkovProcess,
    createEmptyMarkovProcessWithHistory,
    createMarkovModel,
    createMarkovProcess,
    createMarkovProcessWithHistory,
    createSimpleMarkovPolicy,
    fakeUpdateMarkovPolicy,
    getModelOfObservedBehavior,
    updateMarkovExpected,
    updateMarkovPolicy,
    test
  ) where

import Control.Monad (liftM)
import Control.Monad.ST (runST)
import Data.IntMap (IntMap, fromList, keys, (!))
import Data.Ratio ((%))
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq, (|>))
import System.Random (StdGen, getStdGen)
import Text.Show (showListWith)
import qualified Data.Foldable as Fold (toList)
import qualified Data.IntMap as IntMap (elems, insert, lookup, map)
import qualified Data.Sequence as Seq (fromList)
import qualified Data.Set as Set (Set, empty, fold, fromAscList, insert, member)

import LExAu.API.Alphabet (Alphabet(allSymbols), DirectedSymbol, DerivedSymbol, UpdatableAlphabet(possiblyNewerThan))
import LExAu.API.DescriptionLength (DescriptionLength(descriptionLength))
import LExAu.API.Distribution (
    BaseHistogram(scale,weight),
    Distribution(nextMember),
    DistributionFactory(DistributionFactory),
    DistributionType(DistributionType),
    Histogram(increase,increment),
    IntDistribution(allInts,intProbability,hasInt,nextInt),
    LogDistribution(logDistribution)
  )
import LExAu.API.Indexed (Indexed, index, member)
import LExAu.API.Interaction (
    Imitator(offerResponse),
    Interaction,
    Policy(offerAction),
    Recorder(clearHistory,getHistory,record),
    Source(stepCount,takeResponse)
  )
import LExAu.API.Markov (
    MarkovModel(clearUpdated,increaseMarkov,increaseMarkovST,incrementMarkov,incrementMarkovST,updated),
    UpdatedType
  )
import LExAu.API.Model
  ( AlphabetUpdate(..)
  , EvaluateUpdate(..)
  , HasCheckSum(checkSum)
  , ModelOfExpectedBehavior
  , OptimizeUpdate(..)
  , PolicyUpdate(..)
  , Updater(update)
  , UpdaterIO(updateIO)
  , pureUpdateToIO
  )
import LExAu.API.MDL (Optimizer(optimize,updateOptimizer),OptimizerType,createOptimizer)
import LExAu.API.Named (Named, name)
import LExAu.API.ReaderContext (ReaderContext(readsWithContext))
import LExAu.Alphabet.Standard (createAlphabet)
import LExAu.Distribution.Histogram (histogramFromList)
import LExAu.Distribution.MDL (createMDLOptimizer)
import LExAu.Utilities.Logging (Loggable(logs),Summarize(summarize),logVerbose)
import qualified LExAu.API.Distribution as DistributionAPI(DistributionFactory(create))
import qualified LExAu.API.Interaction as Interaction (Imitator(clearUpdated))

type IntDistributionMap distributionType = IntMap (DistributionType distributionType)
data IntDistributionsMap distributionType = IntDistributionsMap { mapping :: IntMap (IntDistributionMap distributionType), missing :: IntDistributionMap distributionType } deriving (Eq, Show, Read)

data ReadModelContext alphabetType symbolType distributionType = ReadModelContext (String -> [String] -> alphabetType) (DistributionFactory distributionType -> alphabetType -> (MarkovModelImpl alphabetType symbolType distributionType)) (DistributionFactory distributionType)

-- |Method to reconstruct a markov model given a factory and a string representation.
instance (
        Read alphabetType,
        Read distributionType
      ) =>
    ReaderContext (ReadModelContext alphabetType symbolType distributionType) (MarkovModelImpl alphabetType symbolType distributionType) where
  readsWithContext (ReadModelContext alphabetFactory histogramFactory distributionFactory) s =
    map (\(m, t) -> (m { factory = distributionFactory }, t)) (reads s)

data MarkovModelImpl alphabetType symbolType distributionType = MarkovModelImpl { alphabet :: alphabetType, updatedEntries :: UpdatedType, factory :: DistributionFactory distributionType, distributionsMap :: (IntDistributionsMap distributionType), maxWeight :: Integer } deriving (Show, Read)

instance (Eq distributionType) => Eq (MarkovModelImpl alphabetType symbolType distributionType) where
  (==) aModel bModel = (distributionsMap aModel) == (distributionsMap bModel)

-- | Creates a new Markov model with the given distribution factory and the given alphabet.
-- Note that the distribution factory needs to create Histograms.
createMarkovModel :: (Histogram alphabetType symbolType distributionType, Alphabet alphabetType symbolType) => (DistributionFactory distributionType) -> alphabetType -> (MarkovModelImpl alphabetType symbolType distributionType)
createMarkovModel factory alphabet =
  MarkovModelImpl alphabet (Set.fromAscList []) factory (IntDistributionsMap (fromList []) (fromList [])) 1

class (Indexed alphabetType symbolType Int) => ShowEntry entryType alphabetType symbolType where
  showEntry :: alphabetType -> UpdatedType -> (Int, entryType, Maybe Int) -> ShowS

class (Indexed alphabetType symbolType Int) => LogEntry entryType alphabetType symbolType where
  logEntry :: alphabetType -> UpdatedType -> (Int, entryType, Maybe Int) -> ShowS

modifiedChar :: UpdatedType -> Int -> Maybe Int -> Char
modifiedChar updated responseKey maybeActionKey =
  if (responseKey, maybeActionKey) `Set.member` updated
    then '*'
    else ' '

instance (
      LogDistribution alphabetType symbolType distributionType,
      Named symbolType
    ) =>
    LogEntry (DistributionType distributionType) alphabetType symbolType where
  logEntry alphabet updated (keyIndex, entry, maybeActionKey) =
    showString "\n    " .
    showChar (modifiedChar updated keyIndex maybeActionKey) .
    showChar ' ' .
    shows (name (alphabet `member` keyIndex)) .
    showChar ':' .
    logDistribution alphabet entry

instance (
      LogDistribution alphabetType symbolType distributionType,
      Named symbolType
    ) =>
    LogEntry (IntDistributionMap distributionType) alphabetType symbolType where
  logEntry alphabet updated (keyIndex, entry, maybeActionKey) =
    showString "\n  " .
    shows (name (alphabet `member` keyIndex)) .
    showChar ':' .
    (showListWith (logEntry alphabet updated) $ map (\x -> (x, entry ! x, maybeActionKey)) (keys entry))

instance (
      LogDistribution alphabetType symbolType distributionType,
      Named symbolType
    ) =>
    LogEntry (IntDistributionsMap distributionType) alphabetType symbolType where
  logEntry alphabet updated (_, IntDistributionsMap mapping missing, _) =
    (showListWith (logEntry alphabet updated) $ map (\x -> (x, mapping ! x, Just x)) (keys mapping)) .
    showString "\n  Nothing:" .
    (showListWith (logEntry alphabet updated) $ map (\x -> (x, missing ! x, Nothing)) (keys missing))

instance (
      LogDistribution alphabetType symbolType distributionType,
      Named symbolType
    ) =>
    Loggable (MarkovModelImpl alphabetType symbolType distributionType) where
  logs (MarkovModelImpl alphabet updated _ mapping maxWeight) =
    showString "{MarkovModelImpl: " .
    shows maxWeight .
    showString ", " .
    logEntry alphabet updated (0, mapping, Nothing) .
    showString "\n  Updated: " .
    shows updated .
    showChar '}'

instance (
      Alphabet alphabetType symbolType,
      Histogram alphabetType symbolType distributionType
    ) =>
    MarkovModel (MarkovModelImpl alphabetType symbolType distributionType) symbolType where

  increaseMarkov model@(MarkovModelImpl _ updated _ oldMapping@(IntDistributionsMap _ missing) oldWeight) lastResponse Nothing nextResponse amount =
    let key = index lastResponse
        maybeOldDistribution = IntMap.lookup key missing
        newDistribution =
          case maybeOldDistribution of
            (Just oldDistribution) ->
              increase oldDistribution nextResponse amount
            Nothing ->
              createDistributionFromList model [(index nextResponse, amount)]
        updatedWeight = weight newDistribution
        newWeight = max oldWeight updatedWeight
        newMissing = IntMap.insert key newDistribution missing
        newUpdated = (key, Nothing) `Set.insert` updated
    in model { updatedEntries = newUpdated, distributionsMap = oldMapping { missing = newMissing }, maxWeight = newWeight }

  increaseMarkov model@(MarkovModelImpl _ updated (DistributionFactory create) oldMappings@(IntDistributionsMap oldMapping _) oldWeight) lastResponse (Just lastAction) nextResponse amount =
    let actionKey = (index lastAction) :: Int
        maybeOldActionMap = IntMap.lookup actionKey oldMapping
        key = (index lastResponse) :: Int
        nextResponseKey = (index nextResponse :: Int)
        (newActionMap, updatedWeight) =
          case maybeOldActionMap of
            (Just oldActionMap) ->
              let maybeOldDistribution = IntMap.lookup key oldActionMap
                  newDistribution = case maybeOldDistribution of
                    (Just oldDistribution) ->
                      increase oldDistribution nextResponse amount
                    Nothing ->
                      createDistributionFromList model [(nextResponseKey, amount)]
              in (IntMap.insert key newDistribution oldActionMap, weight newDistribution)
            Nothing ->
              (fromList [(key, createDistributionFromList model [(nextResponseKey, amount)])], amount)
        newWeight = max oldWeight updatedWeight
        newMapping = IntMap.insert actionKey newActionMap oldMapping
        newUpdated = (key, (Just actionKey)) `Set.insert` updated
    in model { updatedEntries = newUpdated, distributionsMap = oldMappings { mapping = newMapping }, maxWeight = newWeight }

  increaseMarkovST markovRef lastResponse maybeLastAction nextResponse amount =
    do oldModel <- readSTRef markovRef
       newModel <- return $ increaseMarkov oldModel lastResponse maybeLastAction nextResponse amount
       writeSTRef markovRef newModel
       return ()

  incrementMarkovST markovRef lastResponse maybeLastAction nextResponse =
    increaseMarkovST markovRef lastResponse maybeLastAction nextResponse 1

  updated model = updatedEntries model

  clearUpdated model = model { updatedEntries = Set.fromAscList [] }

instance (
      DescriptionLength distributionType
    ) =>
    DescriptionLength (IntDistributionMap distributionType) where

  descriptionLength mapping =
    foldl (+) 0 $ map descriptionLength $ IntMap.elems mapping

instance (
      DescriptionLength distributionType
    ) =>
    DescriptionLength (IntDistributionsMap distributionType) where

  descriptionLength (IntDistributionsMap mapping missing) =
    let mappingDescriptionLength = foldl (+) 0 $ map descriptionLength $ IntMap.elems mapping
        missingDescriptionLength = foldl (+) 0 $ map descriptionLength $ IntMap.elems missing
    in mappingDescriptionLength + missingDescriptionLength

instance (
      DescriptionLength distributionType
    ) =>
    DescriptionLength (MarkovModelImpl alphabetType symbolType distributionType) where

  descriptionLength model = descriptionLength $ distributionsMap model

instance (
      Alphabet alphabetType symbolType,
      UpdatableAlphabet alphabetType
    ) =>
    Updater AlphabetUpdate (MarkovModelImpl alphabetType symbolType distributionType) alphabetType where
  update AlphabetUpdate newAlphabet oldModel =
    if newAlphabet `possiblyNewerThan` (alphabet oldModel)
      then Just $ oldModel { alphabet = newAlphabet }
      else Nothing

createDistributionFromList :: (MarkovModel (MarkovModelImpl alphabetType symbolType distributionType) symbolType) => (MarkovModelImpl alphabetType symbolType distributionType) -> [(Int, Integer)] -> (DistributionType distributionType)
createDistributionFromList model list =
  let Just distributionFactory = DistributionAPI.create $ factory model
  in distributionFactory list

data MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType =
  MarkovProcessImpl {
    theModel :: MarkovModelImpl alphabetType symbolType distributionType,
    history :: Maybe (Seq (DirectedSymbol baseSymbolType)),
    stepCount :: Integer,
    lastResponse :: symbolType,
    lastAction :: (Maybe symbolType),
    random :: StdGen
  } deriving (Read, Show)

instance HasCheckSum (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) where
  checkSum = maxWeight . theModel

data ReadProcessContext alphabetType symbolType distributionType = ReadProcessContext (ReadModelContext alphabetType symbolType distributionType)

-- |Method to reconstruct a markov process given a factory and a string representation.
instance (
        Read alphabetType,
        Read symbolType,
        Read baseSymbolType,
        Read distributionType,
        DerivedSymbol symbolType baseSymbolType
      ) =>
    ReaderContext (ReadProcessContext alphabetType symbolType distributionType) (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) where
  readsWithContext (ReadProcessContext (ReadModelContext alphabetFactory histogramFactory distributionFactory)) s =
    map (\(p, t) -> (replaceDistributionFactory distributionFactory p, t)) (reads s)

replaceDistributionFactory :: (DerivedSymbol symbolType baseSymbolType) => (DistributionFactory distributionType) -> (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) -> (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType)

replaceDistributionFactory distributionFactory process =
  let oldModel = theModel (process)
      newModel = oldModel { factory = distributionFactory }
  in process { theModel = newModel }

instance (
        Alphabet alphabetType symbolType,
        LogDistribution alphabetType symbolType distributionType,
        DerivedSymbol symbolType baseSymbolType
      ) =>
    Loggable (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) where
  logs (MarkovProcessImpl model _ _ lastResponse lastAction stdGen) =
    showString "{MarkovProcessImpl: " .
    logs model .
    showString ",\n  Last action / response: (" .
    logs lastResponse .
    showString ", " .
    logs lastAction .
    showString ")}"

instance (
      Distribution alphabetType symbolType distributionType,
      Alphabet alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    Recorder (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) baseSymbolType where
  getHistory (MarkovProcessImpl _ Nothing _ _ _ _) = []
  getHistory (MarkovProcessImpl _ (Just history) _ _ _ _) = Fold.toList history
  clearHistory (MarkovProcessImpl model _ stepCount lastResponse lastAction random) restartFlag =
    let history = if restartFlag then (Just $ Seq.fromList []) else Nothing
    in MarkovProcessImpl model history stepCount lastResponse lastAction random
  record (MarkovProcessImpl model Nothing stepCount lastReponse lastAction random) _ =
    (MarkovProcessImpl model Nothing stepCount lastReponse lastAction random)
  record (MarkovProcessImpl model (Just history) stepCount lastReponse lastAction random) directedSymbol =
    (MarkovProcessImpl model (Just $ history |> directedSymbol) stepCount lastReponse lastAction random)

instance (
      Distribution alphabetType symbolType distributionType,
      IntDistribution distributionType,
      Alphabet alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    Source (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) alphabetType symbolType where
  takeResponse process@(MarkovProcessImpl model@(MarkovModelImpl alphabet _ _ (IntDistributionsMap mapping missing) _) history stepCount lastResponse maybeLastAction theRandom) =
    let
      actionEntry = getActionEntry model maybeLastAction
      maybeResponseEntry = IntMap.lookup (index lastResponse :: Int) actionEntry
      (response, nextRandom) =
        case maybeResponseEntry of
          (Just responseEntry) ->
            nextMember alphabet responseEntry theRandom
          Nothing ->
            error $ "No response for (response / action): (" ++ (name lastResponse) ++ " / " ++ (show $ liftM name maybeLastAction) ++ "): " ++ (show $ keys actionEntry)
    in (response, (MarkovProcessImpl model history (stepCount + 1) response Nothing nextRandom))
  stepCount (MarkovProcessImpl _ _ stepCount _ _ _) = stepCount

instance (
      DescriptionLength distributionType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    DescriptionLength (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) where

  descriptionLength (MarkovProcessImpl model _ _ _ _ _) =
    descriptionLength model

getActionEntry :: (
      Alphabet alphabetType symbolType
    ) =>
  MarkovModelImpl alphabetType symbolType distributionType -> (Maybe symbolType) -> IntDistributionMap distributionType

getActionEntry (MarkovModelImpl _ _ _ (IntDistributionsMap mapping missing) _) (Just action) =
  let maybeActionEntry = IntMap.lookup (index action :: Int) mapping
  in case maybeActionEntry of
       (Just actionEntry) -> actionEntry
       Nothing -> missing

getActionEntry (MarkovModelImpl _ _ _ (IntDistributionsMap _ missing) _) Nothing = missing

instance (
      Distribution alphabetType symbolType distributionType,
      IntDistribution distributionType,
      Alphabet alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    Policy (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) alphabetType symbolType where
  offerAction process@(MarkovProcessImpl model history stepCount lastResponse _ theRandom) action =
    (MarkovProcessImpl model history (stepCount + 1) lastResponse (Just action) theRandom)

instance (
      Histogram alphabetType symbolType distributionType,
      Alphabet alphabetType symbolType,
      IntDistribution distributionType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    Imitator (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) alphabetType symbolType where
  offerResponse process@(MarkovProcessImpl model history stepCount lastResponse maybeLastAction theRandom) response =
    MarkovProcessImpl (incrementMarkov model lastResponse maybeLastAction response) history (stepCount + 1) response Nothing theRandom
  clearUpdated (MarkovProcessImpl model history stepCount lastResponse maybeLastAction theRandom) =
    MarkovProcessImpl (clearUpdated model) history stepCount lastResponse maybeLastAction theRandom

instance (
      Histogram alphabetType symbolType distributionType,
      Alphabet alphabetType symbolType,
      IntDistribution distributionType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    Interaction (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) alphabetType symbolType where

instance (
        ModelOfExpectedBehavior expectedType,
        Distribution alphabetType symbolType observedDistributionType,
        DerivedSymbol symbolType baseSymbolType,
        BaseHistogram observedDistributionType,
        Eq observedDistributionType
      ) =>
    Updater EvaluateUpdate (MarkovProcessImpl alphabetType symbolType baseSymbolType observedDistributionType) expectedType where
  update EvaluateUpdate _ oldMarkovPolicy = fakeUpdateMarkovPolicy oldMarkovPolicy

instance (
        ModelOfExpectedBehavior expectedType,
        Distribution alphabetType symbolType observedDistributionType,
        DerivedSymbol symbolType baseSymbolType,
        BaseHistogram observedDistributionType,
        Eq observedDistributionType
      ) =>
    UpdaterIO EvaluateUpdate (MarkovProcessImpl alphabetType symbolType baseSymbolType observedDistributionType) expectedType where
  updateIO = pureUpdateToIO update

instance (
      Alphabet alphabetType symbolType,
      UpdatableAlphabet alphabetType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    Updater AlphabetUpdate (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) alphabetType where
  update AlphabetUpdate newAlphabet oldProcess = liftM (\m -> oldProcess { theModel = m }) $ update AlphabetUpdate newAlphabet (theModel oldProcess)

-- |Creates a new MarkovProcess with the given MarkovModel, the given
-- lastResponse and the given state of the random generator.
createMarkovProcess :: (
      Distribution alphabetType symbolType distributionType,
      Alphabet alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    (MarkovModelImpl alphabetType symbolType distributionType) -> symbolType -> StdGen -> (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType)

createMarkovProcess model lastResponse stdGen =
  MarkovProcessImpl model Nothing 0 lastResponse Nothing stdGen

-- |Creates a new MarkovProcess with the given MarkovModel, the given
-- lastResponse and the given state of the random generator. The returned
-- MarkovProcess will record the interactions that it has with its environment.
createMarkovProcessWithHistory :: (
      Distribution alphabetType symbolType distributionType,
      Alphabet alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    (MarkovModelImpl alphabetType symbolType distributionType) -> symbolType -> StdGen -> (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType)

createMarkovProcessWithHistory model lastResponse stdGen =
  MarkovProcessImpl model (Just $ Seq.fromList []) 0 lastResponse Nothing stdGen

-- |Creates new MarkovProcess with an empty model.
createEmptyMarkovProcess :: (
      Histogram alphabetType symbolType distributionType,
      Alphabet alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    alphabetType -> symbolType -> (DistributionFactory distributionType) -> StdGen -> (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType)

createEmptyMarkovProcess alphabet symbol factory stdGen =
  let model = createMarkovModel factory alphabet
  in createMarkovProcess model symbol stdGen

-- |Creates new MarkovProcess with an empty model. The returned
-- MarkovProcess will record the interactions that it has with its environment.
createEmptyMarkovProcessWithHistory :: (
      Histogram alphabetType symbolType distributionType,
      Alphabet alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    alphabetType -> symbolType -> (DistributionFactory distributionType) -> StdGen -> (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType)

createEmptyMarkovProcessWithHistory alphabet symbol factory stdGen =
  let model = createMarkovModel factory alphabet
  in createMarkovProcessWithHistory model symbol stdGen

getModelOfObservedBehavior :: (
      Histogram alphabetType symbolType distributionType,
      Alphabet alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
  (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) -> (MarkovModelImpl alphabetType symbolType distributionType)

getModelOfObservedBehavior (MarkovProcessImpl model _ _ _ _ _) = model

data MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType = MarkovExpectedImpl { observed :: MarkovModelImpl alphabetType symbolType observedDistributionType, expectedDistributionsMap :: (IntDistributionsMap expectedDistributionType), optimizerState :: (OptimizerType observedDistributionType expectedDistributionType oracleType) }

instance ModelOfExpectedBehavior (MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) where

instance (
      Alphabet alphabetType symbolType,
      LogDistribution alphabetType symbolType observedDistributionType,
      LogDistribution alphabetType symbolType expectedDistributionType,
      Summarize (OptimizerType observedDistributionType expectedDistributionType oracleType)
    ) =>
    Loggable (MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) where
  logs (MarkovExpectedImpl (MarkovModelImpl alphabet _ _ _ _) mappings optimizerState) =
    showString "{MarkovExpected: " .
    summarize optimizerState .
    showString ", " .
    logEntry alphabet (Set.fromAscList []) (0, mappings, Nothing) .
    showChar '}'

instance (
        Distribution alphabetType symbolType observedDistributionType,
        BaseHistogram observedDistributionType,
        IntDistribution observedDistributionType,
        Eq expectedDistributionType
      ) =>
    Updater OptimizeUpdate (MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) (MarkovModelImpl alphabetType symbolType observedDistributionType) where
  update OptimizeUpdate updatedMarkovModel oldMarkovExpected = updateMarkovExpected updatedMarkovModel oldMarkovExpected

instance (
      Alphabet alphabetType symbolType,
      UpdatableAlphabet alphabetType
    ) =>
    Updater AlphabetUpdate (MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) alphabetType where
  update AlphabetUpdate newAlphabet oldProcess = liftM (\m -> oldProcess { observed = m }) $ update AlphabetUpdate newAlphabet (observed oldProcess)

createMarkovExpectedInternal ::
    (MarkovModelImpl alphabetType symbolType observedDistributionType)
      -> (DistributionFactory observedDistributionType -> OptimizerType observedDistributionType expectedDistributionType oracleType)
      -> (MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType)

createMarkovExpectedInternal observed optimizerFactory =
  let distributionFactory = factory observed
      optimizer = optimizerFactory distributionFactory
  in MarkovExpectedImpl observed (IntDistributionsMap (fromList []) (fromList [])) optimizer

createMarkovExpected observed = createMarkovExpectedInternal observed createMDLOptimizer

createEmptyMarkovExpected alphabet factory =
  let model = createMarkovModel factory alphabet
  in createMarkovExpected model

updateMarkovExpected :: (
      Eq expectedDistributionType,
      BaseHistogram observedDistributionType,
      IntDistribution observedDistributionType,
      Optimizer (OptimizerType observedDistributionType expectedDistributionType oracleType) observedDistributionType expectedDistributionType
    ) =>
  (MarkovModelImpl alphabetType symbolType observedDistributionType) -> (MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) -> Maybe (MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType)

updateMarkovExpected model@(MarkovModelImpl _ updated _ _ maxWeight) expected =
  let newModel = model { updatedEntries = Set.empty }
      oldOptimizerState = optimizerState expected
      maybeNewOptimizerState = updateOptimizer maxWeight oldOptimizerState
      updatedExpected =
        case maybeNewOptimizerState of
          Nothing -> expected
          Just newOptimizerState -> expected { optimizerState = newOptimizerState }
      startExpected = updatedExpected { observed = newModel }
      (dirtyFlag, nextExpected) = Set.fold (updateMarkovSingleExpected model) (False, startExpected) updated
  in if dirtyFlag
     then Just $ nextExpected
     else Nothing

updateMarkovSingleExpected :: (
      Eq expectedDistributionType,
      BaseHistogram observedDistributionType,
      IntDistribution observedDistributionType
    ) =>
  (MarkovModelImpl alphabetType symbolType observedDistributionType) -> (Int, Maybe Int) -> (Bool, (MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType)) -> (Bool, (MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType))

updateMarkovSingleExpected (MarkovModelImpl _ _ _ (IntDistributionsMap _ observedMissing) _) (responseKey, Nothing) (startDirtyFlag, expected@(MarkovExpectedImpl _ mappings@(IntDistributionsMap _ expectedMissing) optimizerState)) =
  let observedDistribution = observedMissing ! responseKey
      expectedDistribution = optimize optimizerState observedDistribution
      (dirtyFlag, newExpectedMissing) = alterDistribution expectedMissing responseKey expectedDistribution
      newExpectedMappings = mappings { missing = newExpectedMissing }
  in (dirtyFlag, expected { expectedDistributionsMap = newExpectedMappings })

updateMarkovSingleExpected (MarkovModelImpl _ _ _ (IntDistributionsMap observedMapping _) _) (responseKey, Just actionKey) (startDirtyFlag, expected@(MarkovExpectedImpl _ mappings@(IntDistributionsMap expectedMapping _) optimizerState)) =
  let observedDistribution = (observedMapping ! actionKey) ! responseKey
      expectedDistribution = optimize optimizerState observedDistribution
      maybeOldDistributionsMap = IntMap.lookup actionKey expectedMapping
      (dirtyFlag, newDistributionsMap) =
        case maybeOldDistributionsMap of
          (Just oldDistributionsMap) -> alterDistribution oldDistributionsMap responseKey expectedDistribution
          Nothing -> (True, fromList ([(responseKey,expectedDistribution)]))
      newExpectedMapping =
        if dirtyFlag
        then IntMap.insert actionKey newDistributionsMap expectedMapping
        else expectedMapping
      newMappings = mappings { mapping = newExpectedMapping }
  in (dirtyFlag, expected { expectedDistributionsMap = newMappings })

alterDistribution :: (Eq distributionType) => IntMap (DistributionType distributionType) -> Int -> DistributionType distributionType -> (Bool, IntMap (DistributionType distributionType))
alterDistribution oldMapping key newDistribution =
  let maybeOldDistribution = IntMap.lookup key oldMapping
      dirtyFlag =
        case maybeOldDistribution of
          Nothing -> True
          Just oldDistribution -> not (newDistribution == oldDistribution)
      newMapping =
         if dirtyFlag
         then IntMap.insert key newDistribution oldMapping
         else oldMapping
  in (dirtyFlag, newMapping)

createSimpleMarkovPolicy :: (
      Alphabet alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType,
      BaseHistogram distributionType,
      IntDistribution distributionType,
      Histogram alphabetType symbolType distributionType
    ) =>
  (DistributionFactory distributionType) -> alphabetType -> [symbolType] -> StdGen -> (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType)

createSimpleMarkovPolicy factory alphabet actions@(firstAction : _) stdGen =
  let startModel = createMarkovModel factory alphabet
      nextModel =
        runST (
          do ref <- newSTRef startModel
             foldr
               (>>)
               (return (firstAction, firstAction))
               (fmap
                 (\(x, y) ->
                     do incrementMarkovST ref x Nothing y)
                 (squareList actions)
               )
             readSTRef ref
        )
      policy = createMarkovProcess nextModel firstAction stdGen
  in Interaction.clearUpdated policy

squareList :: [a] -> [(a,a)]
squareList list =
  concatMap f list where f = (\x -> zip (repeat x) list)

-- |Updates the actor policy based on the model of expected behavior.
updateMarkovPolicy :: (
      Eq distributionType,
      BaseHistogram distributionType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
  (MarkovExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) -> (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) -> Maybe (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType)

-- Fake implementation.
updateMarkovPolicy _ policy = fakeUpdateMarkovPolicy policy

-- |Updates the actor policy without using the model of expected behavior.
fakeUpdateMarkovPolicy :: (
      Eq distributionType,
      BaseHistogram distributionType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
  (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType) -> Maybe (MarkovProcessImpl alphabetType symbolType baseSymbolType distributionType)

fakeUpdateMarkovPolicy policy =
  let oldModel = theModel policy
      oldMappings = distributionsMap oldModel
      oldMissing = missing oldMappings
      key = (keys oldMissing) !! 1
      oldDistribution = oldMissing ! key
      oldWeight = weight oldDistribution
      newDistribution = scale ((oldWeight + 1) % oldWeight) oldDistribution
      newMissing = IntMap.insert key newDistribution oldMissing
      newMappings = oldMappings { missing = newMissing }
      newModel = oldModel { distributionsMap = newMappings }
  in Just $ policy { theModel = newModel }

-- |Tests the Markov model implementation.
test :: IO ()
test =
  do let alphabet = createAlphabet "trivial" [""]
         empty = alphabet `member` ""
         factory = DistributionFactory $ Just histogramFromList
         emptyModel = createMarkovModel factory alphabet
         model = increaseMarkov emptyModel empty Nothing empty 55
         readModelContext = ReadModelContext createAlphabet createMarkovModel factory
         copyOfModel = readsWithContext readModelContext $ show model
         expected = createEmptyMarkovExpected alphabet factory
         readProcessContext = ReadProcessContext readModelContext
     putStrLn $ ""
     putStrLn $ "Test LExAu.Model.Markov: See test of LExAu.IO.Example"

     logVerbose "Tiny markov model" [logs model]
     logVerbose "Serialized tiny markov model" [shows model]
     logVerbose "Copy of tiny markov model" [shows copyOfModel]
     logVerbose "Empty expected markov" [logs expected]

     stdGen <- getStdGen
     process <- return $ createMarkovProcess model empty stdGen
     logVerbose "Tiny markov process" [shows process]
     copyOfProcess <- return $ readsWithContext readProcessContext $ show process
     logVerbose "Copy of tiny markov process" [shows copyOfProcess]

-- EOF
