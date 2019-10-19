{-# LANGUAGE DeriveDataTypeable #-}
-- |Methods for managing parallel threads of execution.
--
-- Adapted from
-- http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html
module LExAu.Utilities.ThreadManager
    ( ThreadManager()
    , ThreadStatus(..)
    , allThreadIds
    , forkManaged
    , forkNamedManaged
    , forkLoggingManaged
    , getMainLogger
    , getNamedStatus
    , getStatus
    , logErrorOnAbort
    , newManager
    , stopLogger
    , waitFor
    , waitAll
    ) where

import Control.Concurrent
    ( MVar
    , ThreadId
    , forkIO
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    , throwTo
    , tryTakeMVar
    )
import Control.Monad (liftM)
import Control.Exception (Exception, SomeException, try)
import Control.Monad (join)
import Data.Typeable (Typeable, typeOf)
import System.IO (Handle, IOMode(..), stdout)
import qualified Data.Map as M
    ( Map
    , delete
    , elems
    , empty
    , insert
    , keys
    , lookup
    , updateLookupWithKey
    )

import LExAu.Utilities.Logging (Level(..), LogChannel, LogFunction, Logger(), createLogWriter, logFunction)

data ThreadStatus
  = Running
  | Finished              -- terminated normally
  | Threw SomeException   -- killed by uncaught exception
  deriving (Show)

data ThreadInfo = ThreadInfo { theState :: MVar ThreadStatus, theName :: String }

data ThreadManager =
  Mgr
    { theMgrState :: (MVar (M.Map ThreadId ThreadInfo))
    , theLogFactory :: (String -> Logger)
    , theLogWriterThread :: ThreadId
    }

-- | Create a new thread manager.
newManager :: IO ThreadManager

-- | Create a new managed thread.
forkManaged :: ThreadManager -> IO () -> IO ThreadId

-- | Create a new named managed thread.
forkNamedManaged :: ThreadManager -> String -> IO () -> IO ThreadId

-- | Create a new logging managed thread.
forkLoggingManaged :: ThreadManager -> String -> (Logger -> IO ()) -> (LogFunction -> ShowS -> IO ()) -> IO ThreadId

-- | Create a new logging managed thread.
internalForkManaged :: ThreadManager -> String -> (Logger -> IO ()) -> (LogFunction -> ShowS -> IO ()) -> IO ThreadId

-- | The main logger of the thread manager
getMainLogger :: ThreadManager -> Logger

-- | Immediately return the status of a managed thread.
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- | Immediately return the status of a managed thread.
getNamedStatus :: ThreadManager -> ThreadId -> IO (Maybe (String, ThreadStatus))

-- | Block until a specific managed thread terminates.
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- | Block until all managed threads terminate.
waitAll :: ThreadManager -> IO ()

allThreadIds :: ThreadManager -> IO [ThreadId]

newManager =
  do state <- newMVar M.empty
     (logFactory, logger) <- createLogWriter stdout
     logWriterThread <- forkIO logger
     manager <- return $ Mgr { theMgrState = state, theLogFactory = logFactory, theLogWriterThread = logWriterThread }
     return manager

forkManaged manager body =
  forkNamedManaged manager "" body

forkNamedManaged manager nameSpec body = internalForkManaged manager nameSpec (\ h -> body) logErrorOnAbort

forkLoggingManaged manager nameSpec body = internalForkManaged manager nameSpec body

logErrorOnAbort :: LogFunction -> ShowS -> IO ()
logErrorOnAbort logger e =
  logger "ERROR" [e]

internalForkManaged (Mgr mgr logFactory _) nameSpec body onError =
    modifyMVar mgr $ \m -> do
      state <- newEmptyMVar
      tid <- forkIO $
        do let theLogFactory = logFactory nameSpec
           result <- try $ body theLogFactory
           case result of
             Left err ->
               do let logger = logFunction SEVERE theLogFactory nameSpec
                  onError logger (shows err)
             _ -> return ()
           putMVar state (either Threw (const Finished) result)
      name <- return $ if nameSpec == "" then ("Thread #" ++ (show tid)) else nameSpec
      info <- return $ ThreadInfo state name
      return (M.insert tid info m, tid)

getMainLogger manager = theLogFactory manager "MAIN"

getStatus manager tid =
  do namedStatus <- getNamedStatus manager tid
     return $ liftM snd namedStatus

getNamedStatus (Mgr mgr _ _) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m, Nothing)
      Just info ->
        let st = theState info
            name = theName info
        in tryTakeMVar st >>= \mst -> case mst of
                   Nothing -> return (m, Just (name, Running))
                   Just sth -> return (M.delete tid m, Just (name, sth))

waitFor (Mgr mgr _ _) tid =
  join . modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, return Nothing)
      (Just info, m') ->
        let st = theState info
        in (m', Just `fmap` takeMVar st)

waitAll (Mgr mgr _ _) = modifyMVar mgr elems >>= mapM_ (takeMVar . theState)
    where elems m = return (M.empty, M.elems m)

allThreadIds (Mgr mgr _ _) =
  do m <- readMVar mgr
     return $ M.keys m

stopLogger :: ThreadManager -> IO ()

data StopException = StopException deriving (Show, Typeable)
instance Exception StopException

stopLogger manager =
  throwTo (theLogWriterThread manager) StopException

--EOF
