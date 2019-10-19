{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances #-}
module LExAu.Alphabet.Directed (
    DirectedAlphabet,
    createDirectedAlphabet,
    createSymmetricDirectedAlphabet,
    test
  ) where

import LExAu.API.Alphabet(
    Alphabet(addSymbol,addSymbols,allSymbols),
    DerivedSymbol(baseSymbol),
    DirectedSymbol(DirectedSymbol, theDirection, theSymbol),
    SymbolDirection(Action,Response),
    UpdatableAlphabet(possiblyNewerThan),
    directionChar
  )
import LExAu.API.Indexed (
    Collection(allMembers),
    Indexed(member, maybeMember, allIndices),
    IndexedMember(index)
  )
import LExAu.API.Named (Named, name)
import LExAu.Alphabet.Standard (createAlphabet)
import LExAu.Utilities.Logging (Loggable(logs), logVerbose)

data DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType =
  DirectedAlphabetImpl {
    theName :: String,
    theActions :: actionsAlphabetType,
    theResponses :: responsesAlphabetType
  } deriving (Show, Read)

data SymmDirectedAlphabetImpl alphabetType symbolType =
  SymmDirectedAlphabetImpl {
    theSymmName :: String,
    theSymbols :: alphabetType
  } deriving (Show, Read)

class DirectedAlphabet directedAlphabetType symbolType | directedAlphabetType -> symbolType where
  allActions :: directedAlphabetType -> [DirectedSymbol symbolType]
  allResponses :: directedAlphabetType -> [DirectedSymbol symbolType]

instance (
        Alphabet actionsAlphabetType symbolType,
        Alphabet responsesAlphabetType symbolType
      ) =>
    DirectedAlphabet (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) (HiddenSymbolImpl symbolType) where
  allActions directedAlphabet = map ((DirectedSymbol Action) . HiddenSymbolImpl) (allSymbols $ theActions directedAlphabet)
  allResponses directedAlphabet = map ((DirectedSymbol Response) . HiddenSymbolImpl) (allSymbols $ theResponses directedAlphabet)

instance (
        Alphabet alphabetType symbolType
      ) =>
    DirectedAlphabet (SymmDirectedAlphabetImpl alphabetType symbolType) (HiddenSymmSymbolImpl symbolType) where
  allActions directedAlphabet = map ((DirectedSymbol Action) . HiddenSymmSymbolImpl) (allSymbols $ theSymbols directedAlphabet)
  allResponses directedAlphabet = map ((DirectedSymbol Response) . HiddenSymmSymbolImpl) (allSymbols $ theSymbols directedAlphabet)

instance Named (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) where
  name = theName

instance Named (SymmDirectedAlphabetImpl alphabetType symbolType) where
  name = theSymmName

instance (
        Named symbolType,
        Indexed (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) (DirectedSymbol (HiddenSymbolImpl symbolType)) Int,
        Indexed (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) (DirectedSymbol (HiddenSymbolImpl symbolType)) String,
        Alphabet actionsAlphabetType symbolType,
        Alphabet responsesAlphabetType symbolType
      ) =>
    Alphabet (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) (DirectedSymbol (HiddenSymbolImpl symbolType)) where
  addSymbol directedAlphabet symbolName =
    case symbolName of
      '>' : actionName ->
        let (newActions, newAction) = addSymbol (theActions directedAlphabet) actionName
        in (directedAlphabet { theActions = newActions }, (DirectedSymbol Action (HiddenSymbolImpl newAction)))
      '<' : responseName ->
        let (newResponses, newResponse) = addSymbol (theResponses directedAlphabet) responseName
        in (directedAlphabet { theResponses = newResponses }, (DirectedSymbol Response (HiddenSymbolImpl newResponse)))
      _ -> error $ "Unknown direction char: '" ++ symbolName ++ "'"
  addSymbols directedAlphabet symbolNames =
    foldl foldAddSymbol (directedAlphabet, []) symbolNames
  allSymbols directedAlphabet = (allActions directedAlphabet) ++ (allResponses directedAlphabet)

instance (
        UpdatableAlphabet actionsAlphabetType,
        UpdatableAlphabet responsesAlphabetType
      ) =>
    UpdatableAlphabet (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) where
  possiblyNewerThan firstDirectedAlphabet secondDirectedAlphabet =
    ((theActions firstDirectedAlphabet) `possiblyNewerThan` (theActions secondDirectedAlphabet)) ||
      ((theResponses firstDirectedAlphabet) `possiblyNewerThan` (theResponses secondDirectedAlphabet))

instance (
        Named symbolType,
        Indexed (SymmDirectedAlphabetImpl alphabetType symbolType) (DirectedSymbol (HiddenSymmSymbolImpl symbolType)) Int,
        Indexed (SymmDirectedAlphabetImpl alphabetType symbolType) (DirectedSymbol (HiddenSymmSymbolImpl symbolType)) String,
        Alphabet alphabetType symbolType
      ) =>
    Alphabet (SymmDirectedAlphabetImpl alphabetType symbolType) (DirectedSymbol (HiddenSymmSymbolImpl symbolType)) where
  addSymbol directedAlphabet symbolName =
    let directionChar : undirectedSymbolName = symbolName
        (newSymbols, newUndirectedSymbol) = addSymbol (theSymbols directedAlphabet) undirectedSymbolName
        newSymbol =
          case directionChar of
            '>' -> DirectedSymbol Action (HiddenSymmSymbolImpl newUndirectedSymbol)
            '<' -> DirectedSymbol Response (HiddenSymmSymbolImpl newUndirectedSymbol)
            _ -> error $ "Unknown direction char: '" ++ symbolName ++ "'"
    in (directedAlphabet { theSymbols = newSymbols }, newSymbol)
  addSymbols directedAlphabet symbolNames =
    foldl foldAddSymbol (directedAlphabet, []) symbolNames
  allSymbols directedAlphabet = (allActions directedAlphabet) ++ (allResponses directedAlphabet)

instance (
        UpdatableAlphabet alphabetType
      ) =>
    UpdatableAlphabet (SymmDirectedAlphabetImpl alphabetType symbolType) where
  possiblyNewerThan firstDirectedAlphabet secondDirectedAlphabet =
    (theSymbols firstDirectedAlphabet) `possiblyNewerThan` (theSymbols secondDirectedAlphabet)

foldAddSymbol :: (Alphabet alphabetType symbolType) => (alphabetType, [symbolType]) -> String -> (alphabetType, [symbolType])
foldAddSymbol (oldAlphabet, oldExtraSymbols) symbolName =
  let (newAlphabet, extraSymbol) = addSymbol oldAlphabet symbolName
  in (newAlphabet, extraSymbol : oldExtraSymbols)

--A directed alphabet cannot be an instance of a collection, because that would imply that
--directed symbols van only be members of directed alphabets! See Collection ... | memberType -> collectionType 
--instance (DirectedAlphabet directedAlphabetType symbolType) => Collection directedAlphabetType (DirectedSymbol symbolType) where
--  allMembers directedAlphabet = (allActions directedAlphabet) ++ (allResponses directedAlphabet)

--instance (DirectedAlphabet directedAlphabetType symbolType) => Indexed directedAlphabetType (DirectedSymbol symbolType) Int where
data HiddenSymbolImpl symbolType = HiddenSymbolImpl symbolType deriving (Show, Read)
data HiddenSymmSymbolImpl symbolType = HiddenSymmSymbolImpl symbolType deriving (Show, Read)

instance (DerivedSymbol symbolType baseSymbolType) => DerivedSymbol (HiddenSymbolImpl symbolType) baseSymbolType where
  baseSymbol (HiddenSymbolImpl symbol) = baseSymbol symbol

instance (DerivedSymbol symbolType baseSymbolType) => DerivedSymbol (HiddenSymmSymbolImpl symbolType) baseSymbolType where
  baseSymbol (HiddenSymmSymbolImpl symbol) = baseSymbol symbol

instance (Named symbolType) => Named (HiddenSymbolImpl symbolType) where
  name (HiddenSymbolImpl symbol) = name symbol

instance (Named symbolType) => Named (HiddenSymmSymbolImpl symbolType) where
  name (HiddenSymmSymbolImpl symbol) = name symbol

showDirection :: SymbolDirection -> ShowS

showDirection direction = showChar $ directionChar direction

instance (IndexedMember (DirectedSymbol symbolType) Int, DerivedSymbol symbolType baseSymbolType, Named baseSymbolType) => Loggable (DirectedSymbol (DirectedSymbol symbolType)) where
  logs doublyDirectedSymbol =
    let directedSymbol = theSymbol doublyDirectedSymbol
        symbol = baseSymbol $ theSymbol directedSymbol
        direction = theDirection doublyDirectedSymbol
    in showChar '[' .
       showDirection direction .
       shows ((index directedSymbol) :: Int) .
       showChar '.' .
       shows (name symbol) .
       showChar ']'

instance (IndexedMember symbolType Int) => IndexedMember (DirectedSymbol (HiddenSymbolImpl symbolType)) Int where
  index (DirectedSymbol Action (HiddenSymbolImpl symbol)) = 2 * (index symbol)
  index (DirectedSymbol Response (HiddenSymbolImpl symbol)) = (2 * (index symbol)) + 1

instance (IndexedMember symbolType Int) => IndexedMember (DirectedSymbol (HiddenSymmSymbolImpl symbolType)) Int where
  index (DirectedSymbol Action (HiddenSymmSymbolImpl symbol)) = 2 * (index symbol)
  index (DirectedSymbol Response (HiddenSymmSymbolImpl symbol)) = (2 * (index symbol)) + 1

instance (IndexedMember symbolType String) => IndexedMember (DirectedSymbol (HiddenSymbolImpl symbolType)) String where
  index (DirectedSymbol Action (HiddenSymbolImpl symbol)) = '>' : (index symbol)
  index (DirectedSymbol Response (HiddenSymbolImpl symbol)) = '<' : (index symbol)

instance (IndexedMember symbolType String) => IndexedMember (DirectedSymbol (HiddenSymmSymbolImpl symbolType)) String where
  index (DirectedSymbol Action (HiddenSymmSymbolImpl symbol)) = '>' : (index symbol)
  index (DirectedSymbol Response (HiddenSymmSymbolImpl symbol)) = '<' : (index symbol)

instance (
        Named (DirectedSymbol (HiddenSymbolImpl symbolType)),
        IndexedMember (DirectedSymbol (HiddenSymbolImpl symbolType)) Int
      ) =>
    Loggable (DirectedSymbol (HiddenSymbolImpl symbolType)) where
  logs symbol@(DirectedSymbol direction _) =
    showChar '[' .
    showDirection direction .
    shows ((index symbol) :: Int) .
    showChar '.' .
    shows (name symbol) .
    showChar ']'

instance (
        Named (DirectedSymbol (HiddenSymmSymbolImpl symbolType)),
        IndexedMember (DirectedSymbol (HiddenSymmSymbolImpl symbolType)) Int
      ) =>
    Loggable (DirectedSymbol (HiddenSymmSymbolImpl symbolType)) where
  logs symbol@(DirectedSymbol direction _) =
    showChar '[' .
    showDirection direction .
    shows ((index symbol) :: Int) .
    showChar '.' .
    shows (name symbol) .
    showChar ']'

instance (
        Alphabet actionsAlphabetType symbolType,
        Alphabet responsesAlphabetType symbolType,
        DirectedAlphabet (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) (HiddenSymbolImpl symbolType)
      ) =>
    Collection (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) (DirectedSymbol (HiddenSymbolImpl symbolType)) where

  allMembers directedAlphabet = (allActions directedAlphabet) ++ (allResponses directedAlphabet)

instance (
        Alphabet actionsAlphabetType symbolType,
        Alphabet responsesAlphabetType symbolType,
        DirectedAlphabet (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) (HiddenSymbolImpl symbolType)
      ) =>
    Indexed (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) (DirectedSymbol (HiddenSymbolImpl symbolType)) Int where

  member directedAlphabet idx =
    let subidx = idx `div` 2
        (symbol, direction) =
          if idx `rem` 2 == 0
            then ((theActions directedAlphabet) `member` subidx, Action)
            else ((theResponses directedAlphabet) `member` subidx, Response)
    in DirectedSymbol direction $ HiddenSymbolImpl symbol

  maybeMember directedAlphabet idx =
    let subidx = idx `div` 2
        (maybeSymbol, direction) =
          if idx `rem` 2 == 0
            then ((theActions directedAlphabet) `maybeMember` subidx, Action)
            else ((theResponses directedAlphabet) `maybeMember` subidx, Response)
    in case maybeSymbol of
         Just symbol -> Just $ DirectedSymbol direction $ HiddenSymbolImpl symbol
         Nothing -> Nothing

  allIndices directedAlphabet =
    let actionIndices = map (2 *) $ allIndices $ theActions directedAlphabet
        responseIndices = map ((1 +) . (2 *)) $ allIndices $ theResponses directedAlphabet
    in actionIndices ++ responseIndices

instance (
        Alphabet actionsAlphabetType symbolType,
        Alphabet responsesAlphabetType symbolType,
        DirectedAlphabet (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) (HiddenSymbolImpl symbolType)
      ) =>
    Indexed (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) (DirectedSymbol (HiddenSymbolImpl symbolType)) String where

  member directedAlphabet idx =
    let (directionChar : subidx) = idx
        (direction, symbol) = 
          case directionChar of
            '>' -> (Action, (theActions directedAlphabet) `member` subidx)
            '<' -> (Response, (theResponses directedAlphabet) `member` subidx)
    in DirectedSymbol direction $ HiddenSymbolImpl symbol

  maybeMember directedAlphabet idx =
    let (directionChar : subidx) = idx
        (direction, maybeSymbol) = 
          case directionChar of
            '>' -> (Action, (theActions directedAlphabet) `maybeMember` subidx)
            '<' -> (Response, (theResponses directedAlphabet) `maybeMember` subidx)
    in case maybeSymbol of
         Just symbol -> Just $ DirectedSymbol direction $ HiddenSymbolImpl symbol
         Nothing -> Nothing

  allIndices directedAlphabet =
    let actionIndices = map ('>' :) $ allIndices $ theActions directedAlphabet
        responseIndices = map ('<' :) $ allIndices $ theResponses directedAlphabet
    in actionIndices ++ responseIndices

instance (
        Alphabet alphabetType symbolType,
        DirectedAlphabet (SymmDirectedAlphabetImpl alphabetType symbolType) (HiddenSymmSymbolImpl symbolType)
      ) =>
    Collection (SymmDirectedAlphabetImpl alphabetType symbolType) (DirectedSymbol (HiddenSymmSymbolImpl symbolType)) where

  allMembers directedAlphabet = (allActions directedAlphabet) ++ (allResponses directedAlphabet)

instance (
        Alphabet alphabetType symbolType,
        DirectedAlphabet (SymmDirectedAlphabetImpl alphabetType symbolType) (HiddenSymmSymbolImpl symbolType)
      ) =>
    Indexed (SymmDirectedAlphabetImpl alphabetType symbolType) (DirectedSymbol (HiddenSymmSymbolImpl symbolType)) Int where

  member directedAlphabet idx =
    let subidx = idx `rem` 2
        symbol = (theSymbols directedAlphabet) `member` subidx
        direction = if idx `rem` 2 == 0 then Action else Response
    in DirectedSymbol direction $ HiddenSymmSymbolImpl symbol

  maybeMember directedAlphabet idx =
    let subidx = idx `rem` 2
        maybeSymbol = (theSymbols directedAlphabet) `maybeMember` subidx
        direction = if idx `rem` 2 == 0 then Action else Response
    in case maybeSymbol of
         Just symbol -> Just $ DirectedSymbol direction $ HiddenSymmSymbolImpl symbol
         Nothing -> Nothing

  allIndices directedAlphabet =
    let subindices = allIndices $ theSymbols directedAlphabet
        actionIndices = map (2 *) subindices
        responseIndices = map ((1 +) . (2 *)) subindices
    in actionIndices ++ responseIndices

instance (
        Alphabet alphabetType symbolType,
        DirectedAlphabet (SymmDirectedAlphabetImpl alphabetType symbolType) (HiddenSymmSymbolImpl symbolType)
      ) =>
    Indexed (SymmDirectedAlphabetImpl alphabetType symbolType) (DirectedSymbol (HiddenSymmSymbolImpl symbolType)) String where

  member directedAlphabet idx =
    let (directionChar : subidx) = idx
        symbol = (theSymbols directedAlphabet) `member` subidx
        direction = if directionChar == '>' then Action else Response
    in DirectedSymbol direction $ HiddenSymmSymbolImpl symbol

  maybeMember directedAlphabet idx =
    let (directionChar : subidx) = idx
        maybeSymbol = (theSymbols directedAlphabet) `maybeMember` subidx
        direction = if directionChar == '>' then Action else Response
    in case maybeSymbol of
         Just symbol -> Just $ DirectedSymbol direction $ HiddenSymmSymbolImpl symbol
         Nothing -> Nothing

  allIndices directedAlphabet =
    let subindices = allIndices $ theSymbols directedAlphabet
        actionIndices = map ('>' :) subindices
        responseIndices = map ('<' :) subindices
    in actionIndices ++ responseIndices

instance Loggable (DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType) where
  logs directedAlphabet =
    showString "<<<" .
    showString (theName directedAlphabet) .
    showString ">>>"

instance Loggable (SymmDirectedAlphabetImpl alphabetType symbolType) where
  logs directedAlphabet =
    showString "<<=" .
    showString (theSymmName directedAlphabet) .
    showString "=>>"

createDirectedAlphabet :: (Alphabet actionsAlphabetType symbolType, Alphabet responsesAlphabetType symbolType) => String -> actionsAlphabetType -> responsesAlphabetType -> DirectedAlphabetImpl actionsAlphabetType responsesAlphabetType symbolType
createDirectedAlphabet name actions responses = DirectedAlphabetImpl { theName = name, theActions = actions, theResponses = responses }

createSymmetricDirectedAlphabet :: (Alphabet alphabetType symbolType) => String -> alphabetType -> SymmDirectedAlphabetImpl alphabetType symbolType
createSymmetricDirectedAlphabet name alphabet = SymmDirectedAlphabetImpl { theSymmName = name, theSymbols = alphabet }

test :: IO ()
test =
  do let actions = createAlphabet "action" ["left", "right"]
         responses = createAlphabet "response" ["dark", "light"] 
         symmetric = createSymmetricDirectedAlphabet "simple" actions
         directed = createDirectedAlphabet "symbol" actions responses
     logVerbose "Symmetric" [logs symmetric, logs $ allActions symmetric, logs $ allResponses symmetric, logs $ allSymbols symmetric]
     logVerbose "Directed" [logs directed, logs $ allActions directed, logs $ allResponses directed, logs $ allSymbols directed]
