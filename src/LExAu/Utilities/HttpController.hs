{-# LANGUAGE FlexibleContexts #-}

module LExAu.Utilities.HttpController
  ( addCommand
  , getCommands
  , returnCommandValue
  , httpController
  ) where

import Control.Concurrent.Chan
  ( Chan
  , writeChan
  )
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar_
  , readMVar
  )
import Control.Monad (liftM, msum)
import Control.Monad.Trans (liftIO)
-- import Data.JSON2
import Happstack.Server
  ( Method(POST)
  , Request(..)
  , Response
  , ServerPart
  , askRq
  , dir
  , methodOnly
  , notFound
  , nullConf
  , ok
  , simpleHTTP
  , uriRest
  )

import LExAu.Utilities.Logging
  ( Level(..)
  , LogFunction
  , logFunction
  , logVerbose
  )

import LExAu.Utilities.ThreadManager (forkLoggingManaged, logErrorOnAbort, newManager)

data Command =
  Command
    { theModule :: String
    , theCommand :: String
    , theActiveFlag :: Bool
    , theReturnChannel :: Maybe (Chan String)
    }

instance Show (Command) where
  showsPrec _ command =
    showString "Command "
    . shows (theModule command)
    . showString " "
    . shows (theCommand command)
    . showString " "
    . shows (theActiveFlag command)
    . showString " "
    . showString (maybe "Nothing" (\x -> "Just Channel") $ theReturnChannel command)

matchCommand :: String -> String -> Command -> Bool
matchCommand moduleName commandName command = (theModule command) == moduleName && (theCommand command) == commandName

addCommand :: String -> String -> Maybe (Chan String) -> [Command] -> [Command]
addCommand moduleName commandName returnChannel [] = Command moduleName commandName False returnChannel : []
addCommand moduleName commandName returnChannel commands@(firstCommand : otherCommands) =
  if matchCommand moduleName commandName firstCommand
    then commands
    else addCommand moduleName commandName returnChannel otherCommands

activateCommand :: String -> String -> [Command] -> [Command]
activateCommand moduleName commandName [] = []
activateCommand moduleName commandName (firstCommand : otherCommands) =
  if matchCommand moduleName commandName firstCommand
    then (firstCommand { theActiveFlag = True } : otherCommands)
    else firstCommand : (activateCommand moduleName commandName otherCommands)

completedCommand :: String -> String -> [Command] -> [Command]
completedCommand moduleName commandName [] = []
completedCommand moduleName commandName (firstCommand : otherCommands) =
  if matchCommand moduleName commandName firstCommand
    then (firstCommand { theActiveFlag = False } : otherCommands)
    else firstCommand : (completedCommand moduleName commandName otherCommands)

returnCommandValue :: String -> String -> String -> [Command] -> IO [Command]
returnCommandValue moduleName commandName value [] = return []
returnCommandValue moduleName commandName value (firstCommand : otherCommands) =
  if matchCommand moduleName commandName firstCommand
    then do case theReturnChannel firstCommand of
              Just returnChannel -> writeChan returnChannel value
              Nothing -> return ()
            return $ firstCommand { theActiveFlag = False } : otherCommands
    else do tail <- returnCommandValue moduleName commandName value otherCommands
            return $ firstCommand : tail

addToSet :: (Eq a) => [a] -> a -> [a]
addToSet set x =
  if x `elem` set
    then set
    else x : set

getModuleNames :: [Command] -> [String]
getModuleNames commands = foldl addToSet [] $ map theModule commands

getCommandNames :: String -> [Command] -> [String]
getCommandNames moduleName commands = foldl addToSet [] $ map theCommand $ filter ((moduleName ==) . theModule) commands

isActive :: String -> Command -> Bool
isActive moduleName command =
  theModule command == moduleName && theActiveFlag command

getCommands :: MVar [Command] -> String -> IO [String]
getCommands state moduleName =
  do logVerbose "getCommands" [shows moduleName]
     commands <- readMVar state
     return $ map theCommand $ filter (isActive moduleName) commands

updateState :: MVar [Command] -> String -> String -> IO ()
updateState state moduleName commandName =
  modifyMVar_ state (\x -> return $ activateCommand moduleName commandName x)

processCommand :: LogFunction -> MVar [Command] -> String -> String -> a -> ServerPart a
processCommand logger state moduleName commandName response =
  do request <- askRq
     liftIO $
       do logger "Request command" $ map shows $ [moduleName, commandName]
          logger "Request path" $ map shows $ rqPaths request
          updateState state moduleName commandName
     ok response

httpController :: MVar [Command] -> IO ()
httpController state =
  do manager <- newManager
     forkLoggingManaged manager "HTTP controller"
       (\logWrapper ->
           simpleHTTP nullConf $
             do let logger = logFunction FINE logWrapper "[HTTP Controller]"
                    processControllerCommand = processCommand logger state
                liftIO $ logger "Getting commands" []
                commands <- liftIO $ readMVar state
                liftIO $ logger "Commands" $ map shows commands
                do let modules = getModuleNames commands
                   liftIO $ logger "Module names" $ map shows $ ("#" : modules) ++ ["#"]
                   methodOnly [ POST ]
                   msum
                     [ msum $ map (\m -> dir m $ msum $ map (\c -> dir c $ processControllerCommand m c "Command activated") $ getCommandNames m commands) modules
                     , uriRest (\p -> notFound $ "Not found: " ++ p ++ "\n")
                     ]
       )
       logErrorOnAbort
     return ()

--EOF