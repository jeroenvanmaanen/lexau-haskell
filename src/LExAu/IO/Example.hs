{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
-- |Example implementation of an Encounter.
module LExAu.IO.Example (
    createEncounter,
    darkResponse,
    exampleAlphabet,
    exampleEncounter,
    leftAction,
    lightResponse,
    rightAction,
    test
  ) where

import Control.Concurrent.MVar (MVar, tryTakeMVar)
import Control.Monad.ST (ST, runST, stToIO)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import System.Random (RandomGen(split), StdGen, getStdGen)

import LExAu.API.Alphabet (DirectedSymbol(DirectedSymbol),DerivedSymbol(baseSymbol),SymbolDirection(Response),allSymbols)
import LExAu.API.Distribution (DistributionFactory(DistributionFactory))
import LExAu.API.Encounter (
    Encounter(stepCount, stepST, updateStrategyST),
    RecordingEncounter(clearEnvironmentHistory,environmentHistory)
  )
import LExAu.API.Indexed (member)
import LExAu.API.Interaction (Policy(offerAction), Recorder(clearHistory,getHistory,record), Source(takeResponse), offerActionST, takeResponseST)
import LExAu.API.Markov (MarkovModel, increaseMarkovST, incrementMarkovST)
import LExAu.API.Model (PolicyUpdate(..), Updater(update))
import LExAu.API.Named (name)
import LExAu.Alphabet.Standard (createAlphabet)
import LExAu.Alphabet.Directed (createDirectedAlphabet)
import LExAu.Distribution.Histogram (histogramFromList)
import LExAu.Model.Markov (createMarkovModel, createMarkovProcess, createMarkovProcessWithHistory)
import LExAu.Utilities.Logging (Loggable(logs),logVerbose)
import qualified LExAu.API.Interaction as Interaction (Source(stepCount))

data EncounterImpl environmentPolicyType actorPolicyType symbolType =
  EncounterImpl { environment :: environmentPolicyType, actor :: actorPolicyType } deriving (Show, Read)

instance (
      Policy environmentPolicyType alphabetType symbolType,
      Policy actorPolicyType alphabetType symbolType,
      Loggable actorPolicyType
    ) =>
    Encounter (EncounterImpl environmentPolicyType actorPolicyType symbolType) actorPolicyType alphabetType symbolType where

  stepST encounterRef =
    do EncounterImpl oldEnvironmentState oldActorState <- readSTRef encounterRef
       (response, tmpEnvironmentState) <- return $! takeResponse oldEnvironmentState
       tmpActorState <- return $! offerAction oldActorState response
       (action, nextActorState) <- return $! takeResponse tmpActorState
       nextEnvironmentState <- return $! offerAction tmpEnvironmentState action
       writeSTRef encounterRef (EncounterImpl nextEnvironmentState nextActorState)
       return (Just response, Just action, Nothing)

  stepCount (EncounterImpl environmentState _) = (Interaction.stepCount environmentState) `div` 2

  updateStrategyST encounterRef policyPort =
    do oldEncounter <- stToIO $ readSTRef encounterRef
       maybeMaybePolicy <- tryTakeMVar policyPort
       (maybeNewActorPolicy, continue) <- return $
         case maybeMaybePolicy of
           (Just maybePolicy) -> (maybePolicy, case maybePolicy of { Nothing -> False ; _ -> True })
           Nothing -> (Nothing, True)
       case maybeNewActorPolicy of
         (Just newActorPolicy) ->
           do logVerbose "New strategy" [logs newActorPolicy]
              newEncounter <- return $ oldEncounter { actor = newActorPolicy }
              oldEncounter <- stToIO $ writeSTRef encounterRef newEncounter
              return ()
         Nothing -> return ()
       return continue

instance Updater PolicyUpdate (EncounterImpl environmentPolicyType actorPolicyType symbolType) (actorPolicyType) where
  update PolicyUpdate newPolicy oldEncounter = Just $ oldEncounter { actor = newPolicy }

instance (
      Policy environmentPolicyType alphabetType symbolType,
      Policy actorPolicyType alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType,
      Recorder environmentPolicyType baseSymbolType,
      Loggable actorPolicyType
    ) =>
    RecordingEncounter (EncounterImpl environmentPolicyType actorPolicyType symbolType) actorPolicyType alphabetType symbolType baseSymbolType where

  environmentHistory (EncounterImpl environmentPolicy _) =
    getHistory environmentPolicy

  clearEnvironmentHistory oldEncounter@(EncounterImpl environmentPolicy _) flag =
    let newEnvironmentPolicy = clearHistory environmentPolicy flag
    in oldEncounter { environment = newEnvironmentPolicy }

createEncounter :: (
    Policy environmentPolicyType alphabetType symbolType,
    Policy actorPolicyType alphabetType symbolType
  ) =>
  environmentPolicyType -> actorPolicyType -> EncounterImpl environmentPolicyType actorPolicyType symbolType

createEncounter environmentPolicy actorPolicy = EncounterImpl environmentPolicy actorPolicy

actions = createAlphabet "action" ["left", "right"]
responses = createAlphabet "response" ["light", "dark"]
exampleAlphabet = createDirectedAlphabet "symbol" actions responses

-- exampleAlphabet = createAlphabet "ex" ["<light", "<dark", ">left", ">right"]
lightResponse = exampleAlphabet `member` "<light"
darkResponse = exampleAlphabet `member` "<dark"
leftAction = exampleAlphabet `member` ">left"
rightAction = exampleAlphabet `member` ">right"

exampleEncounter stdGen =
  let distributionFactory = (DistributionFactory $ Just histogramFromList)
      startModel = createMarkovModel distributionFactory exampleAlphabet
      (environmentGen, actorGen) = split stdGen
      environmentModel = runST (
        do environmentRef <- newSTRef startModel
           incrementMarkovST environmentRef darkResponse Nothing darkResponse
           incrementMarkovST environmentRef lightResponse Nothing lightResponse
           incrementMarkovST environmentRef lightResponse (Just leftAction) darkResponse
           increaseMarkovST environmentRef lightResponse (Just rightAction) darkResponse 5
           increaseMarkovST environmentRef lightResponse (Just rightAction) lightResponse 4
           incrementMarkovST environmentRef darkResponse (Just leftAction) darkResponse
           incrementMarkovST environmentRef darkResponse (Just rightAction) lightResponse
           readSTRef environmentRef
        )
      actorModel = runST (
        do actorRef <- newSTRef startModel
           incrementMarkovST actorRef leftAction Nothing leftAction
           incrementMarkovST actorRef leftAction Nothing rightAction
           increaseMarkovST actorRef rightAction Nothing leftAction 3
           increaseMarkovST actorRef rightAction Nothing rightAction 3
           readSTRef actorRef
        )
      environmentProcess = createMarkovProcessWithHistory environmentModel darkResponse environmentGen
      actorProcess = createMarkovProcessWithHistory actorModel leftAction actorGen
  in createEncounter environmentProcess actorProcess

test :: IO ()
test = do
  let distributionFactory = (DistributionFactory $ Just histogramFromList)

  putStrLn $ ""
  putStrLn $ "Test LExAu.IO.Example"
  print $ (logs exampleAlphabet) ""
  logVerbose "All symbols" [logs $ allSymbols exampleAlphabet]

  putStrLn $ ""
  startModel <- return $ createMarkovModel distributionFactory exampleAlphabet
  environmentModel <- return $ runST (
    do environmentRef <- newSTRef startModel
       incrementMarkovST environmentRef darkResponse Nothing darkResponse
       incrementMarkovST environmentRef lightResponse Nothing lightResponse
       incrementMarkovST environmentRef lightResponse (Just leftAction) darkResponse
       increaseMarkovST environmentRef lightResponse (Just rightAction) darkResponse 5
       increaseMarkovST environmentRef lightResponse (Just rightAction) lightResponse 4
       incrementMarkovST environmentRef darkResponse (Just leftAction) darkResponse
       incrementMarkovST environmentRef darkResponse (Just rightAction) lightResponse
       readSTRef environmentRef
    )
  logVerbose "Environment model" [logs environmentModel]

  putStrLn $ ""
  stdGen <- getStdGen
  environmentProcess <- return $ createMarkovProcessWithHistory environmentModel darkResponse stdGen
  logVerbose "Environment process" [logs environmentProcess]

  actorModel <- return $ runST (
    do actorRef <- newSTRef startModel
       incrementMarkovST actorRef leftAction Nothing leftAction
       incrementMarkovST actorRef leftAction Nothing rightAction
       incrementMarkovST actorRef rightAction Nothing leftAction
       incrementMarkovST actorRef rightAction Nothing rightAction
       readSTRef actorRef
    )
  logVerbose "Logs actor model" [logs actorModel]
  logVerbose "Shows actor model" [shows actorModel]

  putStrLn $ ""
  actorProcess <- return $ createMarkovProcessWithHistory actorModel leftAction stdGen
  logVerbose "Actor process" [logs actorProcess]

  putStrLn $ ""
  nextProcess <- return $ runST (
    do processRef <- newSTRef environmentProcess
       takeResponseST processRef
       takeResponseST processRef
       offerActionST processRef rightAction
       takeResponseST processRef
       offerActionST processRef rightAction
       takeResponseST processRef
       offerActionST processRef leftAction
       takeResponseST processRef
       readSTRef processRef
    )
  logVerbose "History of next process" $ map logs $ getHistory nextProcess

  putStrLn $ ""
  nextActorProcess <- return $ runST (
    do processRef <- newSTRef actorProcess
       offerActionST processRef darkResponse
       takeResponseST processRef
       takeResponseST processRef
       offerActionST processRef darkResponse
       takeResponseST processRef
       offerActionST processRef darkResponse
       takeResponseST processRef
       offerActionST processRef darkResponse
       takeResponseST processRef
       readSTRef processRef
    )
  logVerbose "History of next actor process" $ map logs $ getHistory nextActorProcess  

  putStrLn $ ""
  encounterStartState <- return $ createEncounter environmentProcess actorProcess
  encounterEndState <- return $ runST (
    do encounterRef <- newSTRef encounterStartState
       stepST encounterRef
       stepST encounterRef
       stepST encounterRef
       stepST encounterRef
       stepST encounterRef
       stepST encounterRef
       stepST encounterRef
       stepST encounterRef
       (EncounterImpl env act) <- readSTRef encounterRef
       envy <- return $ record env (DirectedSymbol Response $ baseSymbol darkResponse)
       writeSTRef encounterRef $ EncounterImpl envy act
       readSTRef encounterRef
    )
  print $ map name $ environmentHistory encounterEndState
  logVerbose "Encounter end state" [shows encounterEndState]

  return ()

-- EOF
