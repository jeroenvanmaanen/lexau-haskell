
module LExAu.Utilities.MVar
  ( EntryContext
  , newEntryContext
  , putMVar
  , readMVar
  , readMVarSummarize
  , swapMVar
  , swapMVarSummarize
  , takeMVar
  , takeMVarSummarize
  , tryPutMVar
  , tryTakeMVar
  , tryTakeMVarSummarize
  ) where

import Control.Monad (liftM)
import Control.Monad.ST (RealWorld, ST, runST, stToIO)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import System.Random (RandomGen(next, split), StdGen, getStdGen)
import qualified Control.Concurrent.MVar as MVar (MVar, putMVar, readMVar, swapMVar, takeMVar, tryPutMVar, tryTakeMVar)

import LExAu.Utilities.Logging
  ( LogFunction
  )

data EntryContext = EntryContext { theLogFunction :: LogFunction, theRandom :: StdGen }

newEntryContext :: MVar.MVar StdGen -> LogFunction -> IO (STRef RealWorld EntryContext)
newEntryContext stdGenMVar logFunction =
  do logFunction "Getting random generator from MVar" []
     (random, newGen) <- liftM split $ MVar.takeMVar stdGenMVar
     logFunction "Putting new random generator in MVar" []
     MVar.putMVar stdGenMVar newGen
     logFunction "Creating initial entry context" []
     context <- return $ EntryContext logFunction random
     logFunction "Returning ST reference to entry context" []
     stToIO $ newSTRef context

putMVar :: STRef RealWorld EntryContext -> ShowS -> MVar.MVar aType -> aType -> IO ()
putMVar contextRef id mvar a =
  entryLog contextRef (const $ showString "()") (showString "Put MVar") id (MVar.putMVar mvar a)

readMVar :: STRef RealWorld EntryContext -> ShowS -> MVar.MVar a -> IO a
readMVar contextRef = takeMVarSummarize contextRef $ const $ showString "something"

readMVarSummarize ::STRef RealWorld EntryContext -> (a -> ShowS) -> ShowS -> MVar.MVar a -> IO a
readMVarSummarize contextRef summarize id mvar =
  entryLog contextRef summarize (showString "Read MVar") id (MVar.readMVar mvar)

swapMVar :: STRef RealWorld EntryContext -> ShowS -> MVar.MVar aType -> aType -> IO aType
swapMVar contextRef = swapMVarSummarize contextRef $ const $ showString "something"

swapMVarSummarize ::STRef RealWorld EntryContext -> (aType -> ShowS) -> ShowS -> MVar.MVar aType -> aType -> IO aType
swapMVarSummarize contextRef summarize id mvar a =
  entryLog contextRef summarize (showString "Swap MVar") id (MVar.swapMVar mvar a)

takeMVar :: STRef RealWorld EntryContext -> ShowS -> MVar.MVar a -> IO a
takeMVar contextRef = takeMVarSummarize contextRef $ const $ showString "something"

takeMVarSummarize ::STRef RealWorld EntryContext -> (a -> ShowS) -> ShowS -> MVar.MVar a -> IO a
takeMVarSummarize contextRef summarize id mvar =
  entryLog contextRef summarize (showString "Take MVar") id (MVar.takeMVar mvar)

tryPutMVar :: STRef RealWorld EntryContext -> ShowS -> MVar.MVar aType -> aType -> IO Bool
tryPutMVar contextRef id mvar a =
  entryLog contextRef shows (showString "Try put MVar") id (MVar.tryPutMVar mvar a)

tryTakeMVar :: STRef RealWorld EntryContext -> ShowS -> MVar.MVar a -> IO (Maybe a)
tryTakeMVar contextRef = tryTakeMVarSummarize contextRef $ const $ showString "something"

tryTakeMVarSummarize :: STRef RealWorld EntryContext -> (a -> ShowS) -> ShowS -> MVar.MVar a -> IO (Maybe a)
tryTakeMVarSummarize contextRef summarize id mvar =
  entryLog contextRef (summarizeMaybe summarize) (showString "Try take MVar") id (MVar.tryTakeMVar mvar)

entryLog :: STRef RealWorld EntryContext -> (a -> ShowS) -> ShowS -> ShowS -> IO a -> IO a
entryLog contextRef summarize prefix id entry =
  do context <- stToIO $ readSTRef contextRef

     oldRandom <- return $ theRandom context
     (token, newRandom) <- return $ next oldRandom
     stToIO $ writeSTRef contextRef $ context { theRandom = newRandom }

     logger <- return $ theLogFunction context
     logger (prefix ": Enter") [id, shows token]
     result <- entry
     logger (prefix ": Exit") [id, shows token, summarize result]
     return result

summarizeMaybe :: (a -> ShowS) -> Maybe a -> ShowS
summarizeMaybe summarize maybeX =
  case maybeX of
    Just x -> showString "Just " . summarize x
    _ -> showString "Nothing"

-- EOF