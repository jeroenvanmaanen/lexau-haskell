{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
-- |Trivial implementation of a Model.
--
-- The alphabet contains a single symbol: the empty string. All interactions
-- are entirely trivial.
module LExAu.Model.Trivial (TrivialModel(TrivialModel), test) where

import Control.Monad.ST (runST, stToIO)
import Data.STRef (newSTRef)

import LExAu.API.Alphabet (Alphabet, DerivedSymbol)
import LExAu.API.Distribution (Distribution)
import LExAu.API.Indexed ((!))
import LExAu.API.Interaction (Imitator, Interaction, Policy, Recorder, Source(stepCount), offerAction, offerActionST, offerResponse, offerResponseST, takeResponse, takeResponseST)
import LExAu.API.Named (name)
import LExAu.Alphabet.Standard (createAlphabet)

-- |A trivial Model based of the given alphabet. The alphabet needs to contain
-- a symbol with the empty name.
data TrivialModel alphabetType = TrivialModel alphabetType deriving (Show, Read)

trivialAlphabet = createAlphabet "trivial" ([""]);
emptySymbol = trivialAlphabet ! ""

instance (
      Alphabet alphabetType symbolType,
      DerivedSymbol symbolType baseSymbolType
    ) => Recorder (TrivialModel alphabetType) baseSymbolType where
instance (
      Alphabet alphabetType symbolType
    ) => Source (TrivialModel alphabetType) alphabetType symbolType where
  takeResponse model@(TrivialModel alphabet) =
    let emptySymbol = alphabet ! ""
    in (emptySymbol, model)
  stepCount model = 0
instance (
      Alphabet alphabetType symbolType
    ) => Policy (TrivialModel alphabetType) alphabetType symbolType where
  offerAction model action = model
instance (
      Alphabet alphabetType symbolType
    ) => Imitator (TrivialModel alphabetType) alphabetType symbolType where
  offerResponse model action = model
instance (
      Alphabet alphabetType symbolType
    ) => Interaction (TrivialModel alphabetType) alphabetType symbolType where

test :: IO ()
test = do
  let emptySymbol = trivialAlphabet ! ""
  modelRef <- stToIO $ newSTRef (TrivialModel trivialAlphabet)
  symbols <- stToIO $
    do x <- takeResponseST modelRef
       offerActionST modelRef emptySymbol
       y <- takeResponseST modelRef
       offerResponseST modelRef emptySymbol
       return [x,y]

  putStrLn ""
  putStrLn $ "Test LExAu.Model.Trivial"
  print $ map (name) symbols

-- EOF
