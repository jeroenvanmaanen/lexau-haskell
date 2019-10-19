{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
-- |Interface module for encounters of LExAu with an environment.
--
-- Use a factory method in an implementation module of this interface to create
-- instances (e.g., LExAu.IO.Example.createEncounter).
module LExAu.API.Encounter (
    Encounter(stepCount,stepIO,stepST,updateStrategy,updateStrategyST),
    RecordingEncounter(clearEnvironmentHistory,environmentHistory)
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.STRef (STRef, newSTRef, readSTRef)

import LExAu.API.Alphabet (Alphabet, DirectedSymbol, DerivedSymbol)
import LExAu.API.Model (PolicyUpdate, Updater)

-- |An encounter between LExAu and an environment.
class (
      Updater PolicyUpdate encounterType policyType,
      Alphabet alphabetType symbolType
    ) =>
    Encounter encounterType policyType alphabetType symbolType
    | encounterType -> alphabetType, encounterType -> symbolType, encounterType -> policyType where

  -- |Performs a single step in the ST Monad
  stepST :: STRef s encounterType -> ST s (Maybe symbolType, Maybe symbolType, Maybe alphabetType)
  stepST encounterRef = error "This encounter cannot be run in an arbitrary ST Monad instance"

  -- |Performs a single step in the IO Monad
  stepIO :: STRef RealWorld encounterType -> IO (Maybe symbolType, Maybe symbolType, Maybe alphabetType)
  stepIO encounterRef = stToIO $ stepST encounterRef

  -- |Returns the number of steps that were taken to get this Encounter from the initial one.
  stepCount :: encounterType -> Integer

  -- | Updates the strategy of the encounter for the given STRef. Returns False if Nothing was returned from the port.
  updateStrategyST :: STRef RealWorld encounterType -> MVar (Maybe policyType) -> IO Bool

  -- | Updates the strategy of the given encounter.
  updateStrategy :: encounterType -> MVar (Maybe policyType) -> IO encounterType

  updateStrategy oldEncounter policyPort =
    do encounterRef <- stToIO $ newSTRef oldEncounter
       updateStrategyST encounterRef policyPort
       stToIO $ readSTRef encounterRef

-- |An encounter that records the exchanged symbols.
class (
      Encounter encounterType policyType alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType
    ) =>
    RecordingEncounter encounterType policyType alphabetType symbolType baseSymbolType where
  -- |Returns the history that was recorded in the environment.
  environmentHistory :: encounterType -> [(DirectedSymbol baseSymbolType)]

  -- |Clears the environment history. Starts (or restarts) recording when the given flag equals true.
  clearEnvironmentHistory :: encounterType -> Bool -> encounterType

-- EOF
