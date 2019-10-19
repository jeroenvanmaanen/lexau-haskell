{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module LExAu.Pipeline.MongoDB
  ( runRecorder
  , test
  ) where

import Control.Concurrent.MVar(newMVar)
import Control.Monad(Functor)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.ST (RealWorld, stToIO)
import Control.Monad.Reader(ReaderT)

import Data.Bson
  ( Document
  , Field((:=))
  )
import Data.Sequence (Seq)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import qualified Data.Bson as Bson
  ( Value(..)
  )

import LExAu.API.Alphabet
  ( SymbolDirection(Action,Response)
  )
import LExAu.API.Encounter
  ( Encounter(stepIO)
  )
import LExAu.API.Model
  ( PolicyUpdate(..)
  , Updater(update)
  )
import LExAu.API.MongoDB
  ( DBContext
  )
import LExAu.API.Named (Named(name))

import LExAu.Utilities.HttpController
  ( addCommand
  , getCommands
  , httpController
  )

import LExAu.Utilities.Logging
  ( LogFunction
  , Loggable(logs)
  , logSilent
  , logVerbose
  )

import LExAu.Utilities.MongoDB
  ( createDbContext
  )

maybeUpdateEncounter ::
    ( Updater PolicyUpdate encounterType policyType
    , DBContext dbContextType
    ) =>
  (dbContextType -> IO (Maybe policyType)) -> STRef RealWorld encounterType -> dbContextType -> IO Bool


maybeUpdateEncounter policyUpdateQuery encounterRef dbContext =
  do maybePolicyUpdate <- policyUpdateQuery dbContext
     case maybePolicyUpdate of
       Just policyUpdate ->
         do oldEncounter <- stToIO $ readSTRef encounterRef
            maybeUpdatedEncounter <- return $ update PolicyUpdate policyUpdate oldEncounter
            case maybeUpdatedEncounter of
              Just updatedEncounter ->
                do stToIO $ writeSTRef encounterRef updatedEncounter
                   return True -- Updated the encounter
              Nothing -> return True -- Update did not return a new encounter
       Nothing -> return True -- No update received

stepToDbIO ::
    ( Encounter encounterType policyType alphabetType symbolType
    , Updater PolicyUpdate encounterType policyType
    , DBContext dbContextType
    ) =>
  LogFunction
  -> dbContextType
  -> (dbContextType -> IO (Maybe policyType))
  -> STRef RealWorld encounterType
  -> (LogFunction -> dbContextType -> SymbolDirection -> Maybe symbolType -> STRef RealWorld (Seq symbolType) -> IO ())
  -> STRef RealWorld (Seq symbolType)
  -> IO (Maybe symbolType, Maybe symbolType)

stepToDbIO logger dbContext policyUpdateQuery encounterRef maybeAddSymbol recorderStateRef =
  do maybeUpdateEncounter policyUpdateQuery encounterRef dbContext
     (maybeResponse, maybeAction, maybeNewAlphabet) <- stepIO encounterRef
     case (maybeNewAlphabet) of
       Nothing -> return ()
       Just newAlphabet ->
         do logger "Publish new alphabet" [shows newAlphabet]
            return ()
     encounter <- stToIO $ readSTRef encounterRef
     case (maybeResponse, maybeAction) of
       (Nothing, Nothing) -> return ()
       _ ->
         do maybeAddSymbol logger dbContext Response maybeResponse recorderStateRef
            maybeAddSymbol logger dbContext Action maybeAction recorderStateRef
     logger "Sent" [shows $ name `fmap` maybeResponse, shows $ name `fmap` maybeAction]
     return $! (maybeResponse, maybeAction)

interactAndPublishUntilNothing ::
    ( Encounter encounterType policyType alphabetType symbolType
    , Updater PolicyUpdate encounterType policyType
    , Loggable symbolType
    , DBContext dbContextType
    ) =>
  LogFunction
  -> IO [String]
  -> String
  -> dbContextType
  -> (dbContextType -> IO (Maybe policyType))
  -> STRef RealWorld encounterType
  -> (LogFunction -> dbContextType -> SymbolDirection -> Maybe symbolType -> STRef RealWorld (Seq symbolType) -> IO ())
  -> STRef RealWorld (Seq symbolType)
  -> Maybe Integer
  -> IO ()

interactAndPublishUntilNothing logger getCommands label dbContext policyUpdateQuery encounterRef maybeAddSymbol recorderStateRef maybeLimit =
  do commands <- getCommands
     if null commands
       then return ()
       else logger "Commands" $ map shows commands
     (continue, newLimit) <- return $
       case maybeLimit of
         (Just limit) ->
           if limit > 0
             then (True, Just $ limit - 1)
             else (False, Just $ 0)
         Nothing -> (True, Nothing)
     continue <-
       return $
         if "stop" `elem` commands
           then False
           else continue
     if continue
       then
         do (maybeResponse, maybeAction) <- stepToDbIO logger dbContext policyUpdateQuery encounterRef maybeAddSymbol recorderStateRef
            logger "Generated symbols" [showString label, logs maybeResponse, logs maybeAction]
            case (maybeResponse, maybeAction) of
              (Nothing, Nothing) -> return ()
              _ -> interactAndPublishUntilNothing logger getCommands label dbContext policyUpdateQuery encounterRef maybeAddSymbol recorderStateRef newLimit
       else return ()

runRecorder ::
    ( Encounter encounterType policyType alphabetType symbolType
    , Loggable symbolType
    , DBContext dbContextType
    ) =>
  Maybe Integer
  -> encounterType
  -> dbContextType
  -> (dbContextType -> IO (Maybe policyType))
  -> (LogFunction -> dbContextType -> SymbolDirection -> Maybe symbolType -> STRef RealWorld (Seq symbolType) -> IO ())
  -> STRef RealWorld (Seq symbolType)
  -> IO ()

runRecorder maybeStepLimit startEncounter dbContext policyUpdateQuery maybeAddSymbol recorderStateRef =
  do let methods = addCommand "recorder" "stop" Nothing []
     encounterRef <- stToIO $ newSTRef startEncounter
     commands <- newMVar methods
     logVerbose "Controller commands" $ map shows $ methods
     theHttpController <- httpController commands
     logVerbose "Created HTTP controller" []
     interactAndPublishUntilNothing logVerbose (getCommands commands "recorder") "(Encounter)" dbContext policyUpdateQuery encounterRef maybeAddSymbol recorderStateRef maybeStepLimit
     return ()

test :: String -> IO ()
test mongoUrl =
  do putStrLn $ ""
     putStrLn $ "Test MongoDB: [" ++ mongoUrl ++ "]"

-- EOF