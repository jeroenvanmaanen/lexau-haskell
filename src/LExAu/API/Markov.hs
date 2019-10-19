{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- |Interface module for Markov Chain models.
--
-- Use a factory method in an implementation module of this interface to create
-- instances (e.g., LExAu.Model.Markov.createMarkovModel).
module LExAu.API.Markov (
    MarkovModel(clearUpdated,increaseMarkov,increaseMarkovST,incrementMarkov,incrementMarkovST,updated),
    UpdatedType
  ) where

import Control.Monad.ST (ST)
import Data.STRef (STRef)
import qualified Data.Set as Set (Set)

import LExAu.API.Alphabet (Alphabet)
import LExAu.API.Distribution (Distribution,DistributionFactory(DistributionFactory),DistributionType)

-- |A set of markers that indicate updated distributions.
type UpdatedType = Set.Set (Int, (Maybe Int))

-- |Methods to update Markov models.
class MarkovModel markovType symbolType | markovType -> symbolType where

  -- |Increases the count for the combination lastResponse-lastAction-nextResponse
  -- by the given amount.
  --
  -- The implementation is of the form:
  -- increaseMarkov oldMarkovModel lastResponse lastAction nextResponse amount = newMarkovModel
  increaseMarkov :: markovType -> symbolType -> (Maybe symbolType) -> symbolType -> Integer -> markovType

  -- |Increases the count for the combination lastResponse-lastAction-nextResponse
  -- by unity.
  --
  -- The implementation is of the form:
  -- incrementMarkov oldMarkovModel lastResponse lastAction nextResponse = newMarkovModel
  incrementMarkov :: markovType -> symbolType -> (Maybe symbolType) -> symbolType -> markovType
  incrementMarkov markov lastResponse maybeLastAction nextResponse =
    increaseMarkov markov lastResponse maybeLastAction nextResponse 1

  -- |Wrapper for increaseMarkov that keeps the model in a STRef.
  increaseMarkovST ::
    STRef s markovType -> symbolType -> (Maybe symbolType) -> symbolType -> Integer -> ST s ()

  -- |Wrapper for incrementMarkov that hides the model in a State Monad.
  incrementMarkovST ::
    STRef s markovType -> symbolType -> (Maybe symbolType) -> symbolType -> ST s ()

  -- |Returns the set of key combinations that point to updated distributions.
  updated :: markovType -> UpdatedType

  -- |Clears the set of markers that indicate updated distributions.
  clearUpdated :: markovType -> markovType

-- EOF
