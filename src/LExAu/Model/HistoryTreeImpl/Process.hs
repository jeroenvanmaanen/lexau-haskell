{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module LExAu.Model.HistoryTreeImpl.Process
  ( HistoryTreeProcessImpl(..)
  , ReadProcessContext(..)
  , createEmptyHistoryTreeProcess
  , createHistoryTreeProcess
  , getModelOfObservedBehavior
  , markProcessExtensibleST
  ) where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq, (<|), (|>))
import System.Random (StdGen)
import qualified Data.Foldable as Fold (toList)
import qualified Data.Sequence as Seq (empty, length, take)

import LExAu.API.Alphabet
  ( Alphabet
  , DerivedSymbol
  , DirectedSymbol(DirectedSymbol, theDirection, theSymbol)
  , SymbolDirection(Action, Response)
  , UpdatableAlphabet
  )
import LExAu.API.Distribution
  ( Distribution
  , DistributionFactory
  , Histogram
  , LogDistribution
  )
import LExAu.API.HistoryTree
  ( HistoryTreeModel
      ( markExtensible
      , incrementHistoryTree
      )
  )
import LExAu.API.Indexed (member)
import LExAu.API.Interaction
  ( Imitator(offerResponse)
  , Interaction
  , Policy(offerAction)
  , Recorder
  , Source(stepCount, takeResponse)
  )
import LExAu.API.Model
  ( AlphabetUpdate(AlphabetUpdate)
  , Updater(update)
  )
import LExAu.API.ReaderContext (ReaderContext(readsWithContext))
import LExAu.Model.HistoryTreeImpl.Observed
  ( HistoryTreeModelImpl(..)
  , ReadModelContext(..)
  , createEmptyHistoryTreeModel
  )
import LExAu.Utilities.Logging (Loggable(logs))

data HistoryTreeProcessImpl alphabetType symbolType distributionType =
  HistoryTreeProcessImpl {
    theModel :: HistoryTreeModelImpl alphabetType symbolType distributionType,
    theState :: Seq (DirectedSymbol symbolType),
    theStepCount :: Integer,
    random :: StdGen
  } deriving (Read, Show)

getModelOfObservedBehavior = theModel

data ReadProcessContext alphabetType symbolType distributionType =
  ReadProcessContext (ReadModelContext alphabetType symbolType distributionType)

-- |Method to reconstruct a history tree process from string representation and given the necessary factories.
instance (
        Read alphabetType,
        Read symbolType,
        Read distributionType
      ) =>
    ReaderContext (ReadProcessContext alphabetType symbolType distributionType) (HistoryTreeProcessImpl alphabetType symbolType distributionType) where
  readsWithContext (ReadProcessContext (ReadModelContext _ _ histogramFactory)) s =
    map (\(p, t) -> (replaceHistogramFactory histogramFactory p, t)) (reads s)

replaceHistogramFactory :: (DistributionFactory distributionType) -> (HistoryTreeProcessImpl alphabetType symbolType distributionType) -> (HistoryTreeProcessImpl alphabetType symbolType distributionType)
replaceHistogramFactory histogramFactory process =
  let oldModel = theModel process
      newModel = oldModel { theFactory = histogramFactory }
  in process { theModel = newModel }

instance (
      Alphabet alphabetType symbolType,
      UpdatableAlphabet alphabetType
    ) =>
    Updater AlphabetUpdate (HistoryTreeProcessImpl alphabetType symbolType distributionType) alphabetType where
  update AlphabetUpdate newAlphabet oldProcess = liftM (\m -> oldProcess { theModel = m }) $ update AlphabetUpdate newAlphabet (theModel oldProcess)

instance (
        Alphabet alphabetType symbolType,
        Loggable (DirectedSymbol symbolType),
        LogDistribution alphabetType symbolType distributionType
      ) =>
    Loggable (HistoryTreeProcessImpl alphabetType symbolType distributionType) where
  logs process =
    let model = theModel process
        state = theState process
    in showString "{HistoryTreeProcessImpl: State=" . (logs state) .
       showString "\n" . logs model .
       showChar '}'

instance (
        DerivedSymbol symbolType baseSymbolType
      ) =>
    Recorder (HistoryTreeProcessImpl alphabetType symbolType distributionType) baseSymbolType where
  -- Only default (non-recording) implementations

instance (
        Alphabet alphabetType symbolType
      ) =>
    Source (HistoryTreeProcessImpl alphabetType symbolType distributionType) alphabetType symbolType where
  -- TODO: provide real implementation
  takeResponse oldProcess =
    let model = theModel oldProcess
        alphabet = theAlphabet model
        response = alphabet `member` (0 :: Int)
    in (response, oldProcess)
  stepCount process = theStepCount process

instance (
        Alphabet alphabetType symbolType
      ) =>
    Policy (HistoryTreeProcessImpl alphabetType symbolType distributionType) alphabetType symbolType where
  offerAction oldProcess action =
    let model = theModel oldProcess
        stateLength = 1 + theMaxDepth model
        oldState = theState oldProcess
        oldStateLength = Seq.length oldState
        tmpState =
          if oldStateLength < stateLength
            then oldState
            else Seq.take (stateLength - 1) oldState
        newState = (DirectedSymbol Action action) <| tmpState
    in oldProcess { theState = newState, theStepCount = (theStepCount oldProcess) + 1 }

instance (
        Alphabet alphabetType symbolType,
        Histogram alphabetType symbolType distributionType
      ) =>
    Imitator (HistoryTreeProcessImpl alphabetType symbolType distributionType) alphabetType symbolType where
  offerResponse oldProcess response =
    let model = theModel oldProcess
        stateLength = 1 + theMaxDepth model
        oldState = theState oldProcess
        oldStateLength = Seq.length oldState
        tmpState =
          if oldStateLength < stateLength
            then oldState
            else Seq.take (stateLength - 1) oldState
        newState = (DirectedSymbol Response response) <| tmpState
        oldModel = theModel oldProcess
        newModel = incrementHistoryTree oldModel (map theSymbol $ Fold.toList oldState) response
    in oldProcess { theModel = newModel, theState = newState, theStepCount = (theStepCount oldProcess) + 1 }
    
instance (
        Alphabet alphabetType symbolType,
        Histogram alphabetType symbolType distributionType
      ) =>
    Interaction (HistoryTreeProcessImpl alphabetType symbolType distributionType) alphabetType symbolType where
  -- Only default implementations

markProcessExtensibleST :: (
      Alphabet alphabetType symbolType,
      Histogram alphabetType symbolType distributionType
    ) =>
  STRef s (HistoryTreeProcessImpl alphabetType symbolType distributionType) -> [symbolType] -> ST s ()

markProcessExtensibleST processRef history =
  do oldProcess <- readSTRef processRef
     newProcess <- return $
       let oldModel = theModel oldProcess
           newModel = markExtensible oldModel history
       in oldProcess { theModel = newModel }
     writeSTRef processRef newProcess
     return ()

createHistoryTreeProcess :: (
      Distribution alphabetType symbolType distributionType,
      Alphabet alphabetType symbolType
    ) =>
  (HistoryTreeModelImpl alphabetType symbolType distributionType) -> StdGen -> (HistoryTreeProcessImpl alphabetType symbolType distributionType)

createHistoryTreeProcess model stdGen =
  (HistoryTreeProcessImpl model Seq.empty 0 stdGen)

createEmptyHistoryTreeProcess :: (
      Histogram alphabetType symbolType distributionType,
      Alphabet alphabetType symbolType
    ) =>
  alphabetType -> (DistributionFactory distributionType) -> StdGen -> (HistoryTreeProcessImpl alphabetType symbolType distributionType)

createEmptyHistoryTreeProcess alphabet factory stdGen =
  (HistoryTreeProcessImpl (createEmptyHistoryTreeModel factory alphabet) Seq.empty 0 stdGen)

-- EOF
