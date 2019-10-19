{-# LANGUAGE FlexibleContexts #-}
module LExAu.Model.HistoryTreeDBImpl.Observed
  ( createRecorderStateRef
  , maybeAddSymbol
  ) where

import qualified Data.Foldable as Fold (toList)

import Control.Monad.ST (RealWorld, stToIO)

import Data.Sequence (Seq, ViewL(EmptyL, (:<)), ViewR(EmptyR, (:>)), (<|), (|>))
import qualified Data.Sequence as Seq (empty, length, null, viewl, viewr)

import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import LExAu.API.Alphabet
  ( Alphabet
  , SymbolDirection(Action,Response)
  )
import LExAu.API.Indexed
  ( IndexedMember(index)
  )

import LExAu.API.MongoDB
  ( DBContext(ensureNode, incrementNodeSymbol)
  )

import LExAu.Utilities.Logging
  ( LogFunction
  , Loggable(logs)
  )

createRecorderStateRef :: IO (STRef RealWorld (Seq symbolType))

createRecorderStateRef = stToIO $ newSTRef Seq.empty

maybeAddSymbol ::
    ( Alphabet alphabetType symbolType
    , IndexedMember symbolType Int
    , Loggable symbolType
    , DBContext dbContextType
    ) =>
  LogFunction
  -> dbContextType
  -> SymbolDirection
  -> Maybe symbolType
  -> STRef RealWorld (Seq symbolType)
  -> IO ()

maybeAddSymbol logger dbContext direction maybeSymbol recorderStateRef =
  case maybeSymbol of
    Nothing -> return ()
    Just symbol ->
      do oldState <- stToIO $ readSTRef recorderStateRef
         do let extend = Seq.length oldState < 3
                extendedState = oldState |> symbol
                newState =
                  if extend
                    then extendedState
                    else
                      case Seq.viewl extendedState of
                        EmptyL -> Seq.empty
                        first :< remainder -> remainder
                oldStateIndexList = fmap index oldState
                symbolIndex = index symbol
            ensureNodes logger dbContext oldStateIndexList
            -- Record the new symbol with the node that the old state points to.
            recordSymbol logger dbContext symbolIndex Seq.empty oldStateIndexList
            logger "New state" $ map logs $ Fold.toList newState
            stToIO $ writeSTRef recorderStateRef newState

ensureNodes :: (DBContext dbContextType) => LogFunction -> dbContextType-> Seq Int -> IO ()

ensureNodes logger dbContext toDo =
  do logger "Ensure nodes" [shows toDo]
     inserted <- ensureNode dbContext toDo
     if Seq.null toDo || not inserted
       then return ()
       else
         do let (newToDo :> _) = Seq.viewr toDo
            ensureNodes logger dbContext newToDo

recordSymbol :: (DBContext dbContextType) => LogFunction -> dbContextType-> Int -> Seq Int -> Seq Int -> IO ()

recordSymbol logger dbContext symbolIndex current toDo =
  do recordSymbolNode logger dbContext symbolIndex current
     if Seq.null toDo
       then return ()
       else
         do let (next :< newToDo) = Seq.viewl toDo
                newCurrent = current |> next
            recordSymbol logger dbContext symbolIndex newCurrent newToDo

recordSymbolNode :: (DBContext dbContextType) => LogFunction -> dbContextType-> Int -> Seq Int -> IO ()

recordSymbolNode logger dbContext symbolIndex path =
  do logger "Record symbol node" [shows $ Fold.toList path, shows symbolIndex] 
     incrementNodeSymbol dbContext path symbolIndex

--EOF