{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
-- |Standard implementation of Alphabets and Symbols.
module LExAu.Alphabet.Standard (createAlphabet, test) where

import Numeric (showInt)
import Data.IntMap (IntMap, elems, fromList, insert, keys, union)
import qualified Control.Exception as E (SomeException, try)
import qualified Data.IntMap as IntMap (empty,lookup)

import LExAu.API.Alphabet (
    Alphabet(addSymbol,addSymbols,allSymbols),
    DerivedSymbol(baseSymbol),
    DirectedSymbol(DirectedSymbol),
    UpdatableAlphabet(possiblyNewerThan),
    directionChar
  )
import LExAu.API.Indexed (Collection(allMembers), Indexed(allIndices, maybeMember, member), IndexedMember(index))
import LExAu.API.Named (Named, name)
import LExAu.Utilities.Logging (Loggable(logs),logListVertically,logVerbose)

type SymbolMap = IntMap SymbolImpl
data AlphabetImpl = Alphabet String SymbolMap Int deriving (Show, Read)
data SymbolImpl = Symbol Int String deriving (Show, Read)

instance Loggable AlphabetImpl where
  logs (Alphabet name _ _) =
        showString "<<" .
        showString name .
        showString ">>"

instance Loggable SymbolImpl where
  logs (Symbol idx name) =
        showChar '[' .
        showInt idx .
        showChar ']' .
        showString name

instance Loggable (DirectedSymbol SymbolImpl) where
  logs (DirectedSymbol direction symbol) =
    showChar (directionChar direction) . logs symbol

instance Named AlphabetImpl where
  name (Alphabet name _ _) = name

instance Named SymbolImpl where
  name (Symbol _ name) = name

instance Alphabet AlphabetImpl SymbolImpl where
  addSymbol (Alphabet name symbolMap nextIndex) symbolName =
    let symbol = createSymbol nextIndex symbolName
    in (Alphabet name (insert nextIndex symbol symbolMap) (nextIndex + 1), symbol)
  addSymbols (Alphabet name currentSymbolMap startIndex) symbolNames =
    let (extraSymbolMap, nextIndex) = createSymbolMap startIndex symbolNames
        symbolMap = currentSymbolMap `union` extraSymbolMap
    in (Alphabet name symbolMap nextIndex, elems extraSymbolMap)
  allSymbols (Alphabet _ symbolMap _) = elems symbolMap

instance UpdatableAlphabet AlphabetImpl where
  possiblyNewerThan (Alphabet _ _ firstNextIndex) (Alphabet _ _ secondNextIndex) =
    firstNextIndex > secondNextIndex

instance Collection AlphabetImpl SymbolImpl where
  allMembers (Alphabet _ symbolMap _) = elems symbolMap

instance Indexed AlphabetImpl SymbolImpl Int where
  maybeMember (Alphabet _ symbolMap _) idx = IntMap.lookup idx symbolMap
  allIndices (Alphabet _ symbolMap _) = keys symbolMap

instance IndexedMember SymbolImpl Int where
  index (Symbol idx _) = idx

instance Indexed AlphabetImpl SymbolImpl String where
  member alphabet symbolName =
    case maybeMember alphabet symbolName of
      Just result -> result
      Nothing -> error $
        (showString "LExAu.Alphabet.Standard: " .
         logs alphabet .
         showString ": symbol not found in list: " .
         showString symbolName) ""
  maybeMember alphabet@(Alphabet _ symbolMap _) symbolName = getSymbolFromList alphabet (elems symbolMap) symbolName
  allIndices (Alphabet _ symbolMap _) = map name $ elems symbolMap

instance IndexedMember SymbolImpl String where
  index (Symbol _ name) = name

instance DerivedSymbol SymbolImpl SymbolImpl where
  baseSymbol = id

getSymbolFromList :: AlphabetImpl -> [SymbolImpl] -> String -> Maybe SymbolImpl
getSymbolFromList alphabet (theSymbol@(Symbol _ thisSymbolName) : remainingSymbols) symbolName =
  if thisSymbolName == symbolName
  then Just theSymbol
  else getSymbolFromList alphabet remainingSymbols symbolName
getSymbolFromList alphabet [] symbolName = Nothing

-- |Creates a new alphabet that contains a symbol for each name in the given
-- list of names.
createAlphabet :: String -> [String] -> AlphabetImpl
createAlphabet name symbolNames =
  let (symbolMap, nextIndex) = createSymbolMap 1 symbolNames
  in (Alphabet name symbolMap nextIndex)

createSymbol :: Int -> String -> SymbolImpl
createSymbol idx name = (Symbol idx name)

createSymbolMap :: Int -> [String] -> (SymbolMap, Int)
createSymbolMap firstIndex [] = (IntMap.empty, firstIndex)
createSymbolMap firstIndex symbolNames =
  let
    symbolList = map (\(idx, symbolName) -> (idx, (createSymbol idx symbolName))) (zip [firstIndex..] symbolNames)
    nextIndex = (fst $ last symbolList) + 1
    symbolMap = fromList symbolList
  in (symbolMap, nextIndex)

-- |Tests the standard alphabet implementation.
test :: IO ()
test = do
  let alphabet = createAlphabet "plankje" ["aap", "noot", "mies"]
      mies = alphabet `member` "mies"
      (extended, extra) = alphabet `addSymbols` ["piep", "kraak"]
      copyOfExtended = read $ show extended :: AlphabetImpl

  putStrLn ""
  putStrLn $ "Test LExAu.Alphabet.Standard"
  logVerbose "Alphabet" [logs alphabet]
  logVerbose "All symbols" [logListVertically $ allSymbols alphabet]
  print $ (allIndices alphabet :: [Int])
  logVerbose "Member 'aap'" [logs $ alphabet `member` "aap"]
  logVerbose "Member 'mies'" [logs $ alphabet `member` "mies"]
  logVerbose "Member 3" [logs $ alphabet `member` (fromInteger 3 :: Int)]

  result <- E.try (return $! alphabet `member` "piep")
  putStrLn $ case result of
    (Left exception) -> "Correct: " ++ (show $ (exception :: E.SomeException))
    (Right symbol) -> (showString "ERROR: got non existent symbol: " . logs symbol) ""

  putStrLn ""
  logVerbose "Extended" [logs extended]
  logVerbose "All symbols" [logListVertically $ allSymbols extended]
  logVerbose "Extra symbols" [logListVertically $ extra]
  logVerbose "Member 'piep'" [logs $ extended `member` "piep"]
  logVerbose "Member 'kraak'" [logs $ extended `member` "kraak"]
  logVerbose "Shows extended" [shows extended]
  logVerbose "Shows copy of extended" [shows copyOfExtended]

-- EOF
