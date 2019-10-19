{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- |Interface module for models of interaction.
--
-- Use a factory method in an implementation module of this interface to create
-- instances (e.g., LExAu.Model.Markov.createMarkovProcess).
module LExAu.API.Interaction (
    Imitator(clearUpdated,offerResponse),
    Interaction(offerDirectedSymbol),
    Policy(offerAction),
    Recorder(clearHistory,getHistory,record),
    Source(stepCount,takeResponse),
    offerActionST,
    offerDirectedSymbolST,
    offerResponseST,
    takeResponseST
  ) where

import Control.Monad.ST(ST)
import Data.STRef(STRef,readSTRef,writeSTRef)

import LExAu.API.Alphabet (
    Alphabet,
    DirectedSymbol(DirectedSymbol, theDirection, theSymbol),
    DerivedSymbol(baseSymbol),
    SymbolDirection(Action,Response)
  )

-- |Record and retrieve a history of the interaction.
class Recorder sourceType baseSymbolType | sourceType -> baseSymbolType where
  -- |Records a symbol in the history.
  record :: sourceType -> DirectedSymbol baseSymbolType -> sourceType
  record source _ = source

  -- |Returns the recorded history.
  getHistory :: sourceType -> [(DirectedSymbol baseSymbolType)]
  getHistory _ = []

  -- |Clears the history. Starts (or restarts) recording the history if the given boolean is true.
  clearHistory :: sourceType -> Bool -> sourceType
  clearHistory source _ = source

-- |A source of symbols.
class (
      Alphabet alphabetType symbolType
    ) =>
    Source sourceType alphabetType symbolType
    | sourceType -> symbolType where
  -- |Returns the next response together with the next state of the Source.
  -- This method is implemented by the implementation modules. Modules that use
  -- this API preferably use the ST method in an appropriate Monad.
  takeResponse :: sourceType -> (symbolType, sourceType)

  -- |Returns the number of steps that were taken to get this Source from the initial one.
  stepCount :: sourceType -> Integer

-- |A source of symbols that can also consume symbols. Typically an imitator
-- is first offered a stream of symbols and will then produce symbols with a
-- similar statistical pattern.
class (
      Source sourceType alphabetType symbolType
    ) =>
    Imitator sourceType alphabetType symbolType where
  -- |Offers the next response to the Imitator and returns the next state of the Imitator.
  -- This method is implemented by the implementation modules. Modules that use
  -- this API preferably use the offerResponseST method in an appropriate Monad.
  offerResponse :: sourceType -> symbolType -> sourceType
  clearUpdated :: sourceType -> sourceType
  clearUpdated source = source

-- |A source of symbols that responds to actions offered to it.
class (
      Source sourceType alphabetType symbolType
    ) =>
    Policy sourceType alphabetType symbolType where
  -- |Offers the next action to the Policy and returns the next state of the Policy.
  -- This method is implemented by the implementation modules. Modules that use
  -- this API preferably use the offerActionST method in an appropriate Monad.
  offerAction :: sourceType -> symbolType -> sourceType

-- |A policy that can also consume symbols.
class (
      Imitator sourceType alphabetType symbolType,
      Policy sourceType alphabetType symbolType
    ) =>
    Interaction sourceType alphabetType symbolType where
  offerDirectedSymbol :: sourceType -> (DirectedSymbol symbolType) -> sourceType
  offerDirectedSymbol model (DirectedSymbol Action symbol) = offerAction model symbol
  offerDirectedSymbol model (DirectedSymbol Response symbol) = offerResponse model symbol

-- |A wrapper around method takeResponse that keeps the Source in a STRef.
takeResponseST :: (
    Alphabet alphabetType symbolType,
    Source sourceType alphabetType symbolType,
    DerivedSymbol symbolType baseSymbolType,
    Recorder sourceType baseSymbolType
  ) =>
  STRef s sourceType -> ST s symbolType

takeResponseST sourceRef =
  do theModel <- readSTRef sourceRef
     (symbol, tmpModel) <- return $ takeResponse theModel
     nextModel <- return $ record tmpModel (DirectedSymbol Response $ baseSymbol symbol)
     writeSTRef sourceRef nextModel
     return symbol

-- |A wrapper around method offerAction that keeps the Policy in a STRef.
offerActionST :: (
    Alphabet alphabetType symbolType,
    Policy policyType alphabetType symbolType,
    DerivedSymbol symbolType baseSymbolType,
    Recorder policyType baseSymbolType
  ) =>
  STRef s policyType -> symbolType -> ST s ()

offerActionST sourceRef action =
  do theModel <- readSTRef sourceRef
     tmpModel <- return $ offerAction theModel action
     nextModel <- return $ record tmpModel (DirectedSymbol Action $ baseSymbol action)
     writeSTRef sourceRef nextModel
     return ()

-- |A wrapper around method offerResponse that keeps the Imitator in a STRef.
offerResponseST :: (
    Alphabet alphabetType symbolType,
    Imitator imitatorType alphabetType symbolType,
    DerivedSymbol symbolType baseSymbolType,
    Recorder imitatorType baseSymbolType
  ) =>
  STRef s imitatorType -> symbolType -> ST s ()

offerResponseST sourceRef response =
  do theModel <- readSTRef sourceRef
     tmpModel <- return $ offerResponse theModel response
     nextModel <- return $ record tmpModel (DirectedSymbol Response $ baseSymbol response)
     writeSTRef sourceRef nextModel
     return ()

-- |A wrapper around method offerDirectedSymbol that keeps the Interaction in a STRef.
offerDirectedSymbolST :: (
    Alphabet alphabetType symbolType,
    Interaction modelType alphabetType symbolType,
    DerivedSymbol symbolType baseSymbolType,
    Recorder modelType baseSymbolType
  ) =>
  STRef s modelType -> (DirectedSymbol symbolType) -> ST s ()

offerDirectedSymbolST sourceRef directedSymbol =
  do theModel <- readSTRef sourceRef
     tmpModel <- return $ offerDirectedSymbol theModel directedSymbol
     nextModel <- return $ record tmpModel (DirectedSymbol (theDirection directedSymbol) (baseSymbol directedSymbol))
     writeSTRef sourceRef nextModel
     return ()

-- EOF
