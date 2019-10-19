{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
-- |Interface module for alphabets and symbols.
--
-- Use a factory method in an implementation module of this interface to create
-- instances (e.g., LExAu.Alphabet.Standard.createAlphabet).
module LExAu.API.Alphabet (
    Alphabet(addSymbol,addSymbols,allSymbols),
    DirectedSymbol(DirectedSymbol,theDirection,theSymbol),
    DerivedSymbol(baseSymbol),
    SymbolDirection(Action,Response),
    UpdatableAlphabet(possiblyNewerThan),
    directionChar,
    reverseSymbolDirection
  ) where

import LExAu.API.Indexed (Indexed, IndexedMember(index))
import LExAu.API.Named (Named, name)
import LExAu.Utilities.Logging (Loggable(logs))

-- |Enumeration of kinds of symbols.
data SymbolDirection = Action | Response deriving (Show, Read)
data DirectedSymbol symbolType = DirectedSymbol { theDirection :: !SymbolDirection, theSymbol :: !symbolType } deriving (Show, Read)

-- |Operations on alphabets and symbols.
class (
      Named alphabetType,
      Named symbolType,
      Indexed alphabetType symbolType Int,
      IndexedMember symbolType Int,
      Indexed alphabetType symbolType String,
      IndexedMember symbolType String
    ) =>
    Alphabet alphabetType symbolType
    where

  -- |Adds a symbol to the given alphabet.
  addSymbol :: alphabetType -> String -> (alphabetType, symbolType)

  -- |Adds a number of symbols to the given alphabet.
  addSymbols :: alphabetType -> [String] -> (alphabetType, [symbolType])

  -- |Returns all symbols in a given alphabet.
  allSymbols :: alphabetType -> [symbolType]

class UpdatableAlphabet alphabetType where
  -- |Returns False when the first alphabet cannot possibly be newer than the second alphabet
  possiblyNewerThan :: alphabetType -> alphabetType -> Bool
  possiblyNewerThan firstAlphabet secondAlphabet = True

class DerivedSymbol derivedSymbolType baseSymbolType | derivedSymbolType -> baseSymbolType where
  baseSymbol :: derivedSymbolType -> baseSymbolType

instance (
        DerivedSymbol symbolType baseSymbolType
      ) => DerivedSymbol (DirectedSymbol symbolType) baseSymbolType where
  baseSymbol = baseSymbol . theSymbol

instance (Named symbolType) => Named (DirectedSymbol symbolType) where
  name (DirectedSymbol direction symbol) = (directionChar direction) : (name symbol)

directionChar :: SymbolDirection -> Char
directionChar Action = '>'
directionChar Response = '<'

reverseSymbolDirection :: SymbolDirection -> SymbolDirection
reverseSymbolDirection Action = Response
reverseSymbolDirection Response = Action

symbolReverseDirection :: DirectedSymbol symbolType -> DirectedSymbol symbolType
symbolReverseDirection (DirectedSymbol direction symbol) = (DirectedSymbol (reverseSymbolDirection direction) symbol)

-- EOF
