{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module LExAu.Utilities.MongoDB
  ( createDbContext
  , test
  ) where

import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.Reader(ReaderT)

import Data.Bson
  ( Document
  , Field((:=))
  , (=:)
  )

import qualified Data.Bson as Bson
  ( Value(..)
  )

import qualified Data.Foldable as Fold (toList)

import Data.Maybe (isJust)

import Data.Sequence (Seq, ViewL(EmptyL, (:<)), ViewR(EmptyR, (:>)))
import qualified Data.Sequence as Seq (length, viewl, viewr)

import Data.UString
  ( UString
  , u
  )

import qualified Database.MongoDB as MDB
  ( Access
  , Action
  , ConnPool
  , Cursor
  , Database(Database)
  , Failure(..)
  , Host
  , Index(Index)
  , MasterOrSlaveOk(Master,SlaveOk)
  , Selection(Select)
  , access
  , allDatabases
  , delete
  , ensureIndex
  , find
  , insert
  , modify
  , newConnPool
  , next
  , readHostPort
  , rest
  , select
  , safe
  , use
  )

import LExAu.API.MongoDB (DBContext(..))

import LExAu.Utilities.Logging
  ( logVerbose
  )

data DBContextImpl = DBContextImpl
  { theDatabase :: MDB.Database
  , theConnPool :: MDB.ConnPool MDB.Host
  }

data NodeKey = NodeKey | NodePath | NodePathDepth | NodeDistribution | NodeModelVersion

data LogMode = ReportAll | IgnoreDuplicate

nodeCollectionName = "nodes" :: UString

nodeKeyName :: NodeKey -> UString

nodeKeyName nodeKey =
  case nodeKey of
    NodeKey -> "key"
    NodePath -> "path"
    NodePathDepth -> "path-depth"
    NodeDistribution -> "distribution"
    NodeModelVersion -> "model-version"

zero = Bson.Int64 0
unity = Bson.Int64 1

instance DBContext DBContextImpl where
  ensureNode dbContext path =
    do let key = u $ stateToKey path ""
           pathDepth = fromIntegral $ Seq.length path
           pathList = Fold.toList path
           document =
             [ nodeKeyName NodeKey =: key
             , nodeKeyName NodePath =: pathList
             , nodeKeyName NodePathDepth =: (pathDepth :: Int)
             , nodeKeyName NodeDistribution := Bson.Doc []
             , nodeKeyName NodeModelVersion := zero
             ]
       logVerbose "Ensure node" [shows path, shows key, shows pathDepth, shows document]
       result <- dbAction "In 'ensureNode': insert" IgnoreDuplicate $ dbAccess dbContext $ MDB.insert nodeCollectionName document
       dbAction "In 'ensureNode': siblings" ReportAll $ dbAccess dbContext $ testLogSiblings dbContext path
       return $ isJust result
  incrementNodeSymbol dbContext path symbolIndex =
    do let key = u $ stateToKey path ""
           selection = MDB.Select [ nodeKeyName NodeKey =: key ] nodeCollectionName
           fieldName = u $ "distribution." ++ (show $ fromIntegral symbolIndex)
           modifier = [ "$inc" := Bson.Doc [ fieldName := unity ] ]
       logVerbose "Increment node symbol" [shows $ Fold.toList path, shows symbolIndex, shows selection, shows modifier]
       dbAction "In 'incrementNodeSymbol'" ReportAll $ dbAccess dbContext $ MDB.modify selection modifier
       return ()

dbAccess :: (MonadIO m) => DBContextImpl -> ReaderT MDB.Database (MDB.Action m) a -> m (Either MDB.Failure a)

dbAccess dbContext action =
  MDB.access MDB.safe MDB.Master (theConnPool dbContext) $ MDB.use (theDatabase dbContext) action

ensureIndices :: DBContextImpl -> IO ()

ensureIndices dbContext =
  do let pathIndex =
           MDB.Index
             nodeCollectionName -- Collection name
             [ nodeKeyName NodeModelVersion =: (1 :: Int)
             , nodeKeyName NodePathDepth =: (1 :: Int)
             , nodeKeyName NodeKey =: (1 :: Int)
             ] -- Order
             "node-key-index" -- Index name
             True -- Unique
             True -- Drop dups
     dbAction "In 'ensureIndices'" ReportAll $ dbAccess dbContext $ MDB.ensureIndex pathIndex
     return ()

createDbContext :: String -> IO DBContextImpl

createDbContext mongoUrl =
  do let hostPortString = takeWhile (/= '/') mongoUrl
         hostPort = MDB.readHostPort hostPortString
     connPool <- MDB.newConnPool 1 hostPort
     do let dbContext = DBContextImpl (MDB.Database "lexau") connPool
        ensureIndices dbContext
        return dbContext

stateToKey :: Seq Int -> ShowS

stateToKey state =
  case Seq.viewr state of
    EmptyR -> id
    (init :> a) -> shows a . statePrefixToKey init

statePrefixToKey state =
  case Seq.viewr state of
    EmptyR -> id
    (init :> a) -> showString ", " . shows a . statePrefixToKey init

dbAction :: (MonadIO m) => String -> LogMode -> m (Either MDB.Failure t) -> m (Maybe t)

dbAction message logMode action =
  do let prefix =
           if null message
             then id
             else (showString message :)
     result <- action
     case result of
       Left failure ->
         do let report =
                  case (logMode, failure) of
                    (IgnoreDuplicate, MDB.WriteFailure 11000 _) -> False
                    _ -> True
            if report
              then liftIO $ logVerbose "Database error" $ prefix [showString "[ ", shows failure, showString " ]"]
              else return ()
            return Nothing
       Right value -> return $ Just value

testLogSiblings :: (MDB.Access m) => DBContextImpl -> Seq Int -> m ()

testLogSiblings dbContext path =
  do let pathDepth = (fromIntegral $ Seq.length path) :: Int
         addPathCheck =
           if pathDepth <= 1
             then id
             else
               let (_ :< parentPath) = Seq.viewl path
                   siblingKeyRegex = u $ (showChar '^' . (stateToKey parentPath)) $ ","
                   clause = nodeKeyName NodeKey =: ["$regex" =: siblingKeyRegex]
               in (clause :)
         query = addPathCheck [nodeKeyName NodePathDepth =: pathDepth]
     liftIO $ logVerbose "Test log siblings" [shows query]
     maybeResult <- dbAction "" ReportAll $ dbAccess dbContext $ MDB.find (MDB.select query nodeCollectionName)
     case maybeResult of
        Nothing -> liftIO $ logVerbose "ERROR: no siblings" [shows path]
        Just cursor ->
          do liftIO $ logVerbose "Siblings" [showString "BEGIN"]
             logResults cursor
             liftIO $ logVerbose "Siblings" [showString "END"]
     return ()

logResults :: (MDB.Access m) => MDB.Cursor -> m ()

logResults cursor =
  do maybeDocument <- MDB.next cursor
     case maybeDocument of
       Nothing -> return ()
       Just document ->
         do liftIO $ logVerbose "Result" [shows document]
            logResults cursor

testGlobalAction :: (MonadIO m) => m (Either MDB.Failure a) -> (a -> IO ()) -> m ()
testGlobalAction action report =
  do result <- dbAction "In 'testGlobalAction'" ReportAll action
     case result of
       Just value -> liftIO $ report value
       _ -> return ()

testAction :: (MonadIO m) => DBContextImpl -> ReaderT MDB.Database (MDB.Action m) a -> (a -> IO ()) -> m ()
testAction dbContext action report = testGlobalAction (dbAccess dbContext action) report

testAllAction :: (MonadIO m, Functor m) => DBContextImpl -> ReaderT MDB.Database (MDB.Action m) MDB.Cursor -> ([Document] -> IO ()) -> m ()
testAllAction dbContext action report = testGlobalAction (dbAccess dbContext $ action >>= MDB.rest) report

test :: String -> IO ()
test mongoUrl =
  do let hostPortString = takeWhile (/= '/') mongoUrl
         hostPort = MDB.readHostPort hostPortString
         database = tail $ dropWhile (/= '/') mongoUrl
         uTest = "//effe/wat/anders" :: UString
         otherQuery = ["key" =: uTest]
         otherDoc = otherQuery ++ [ "depth" =: (1000 :: Int), "path" =: [ uTest ] ]

     putStrLn $ ""
     putStrLn $ "Test MongoDB: [" ++ mongoUrl ++ "]: [" ++ hostPortString ++ "]: [" ++ database ++ "]"
     connPool <- MDB.newConnPool 1 hostPort

     dbContext <- createDbContext mongoUrl

     testGlobalAction (MDB.access MDB.safe MDB.Master connPool MDB.allDatabases) (logVerbose "MongoDB databases" . map shows)
     testAllAction dbContext (MDB.find $ MDB.select [] nodeCollectionName) (logVerbose "MongoDB nodes" . map shows)
     testAction dbContext (MDB.delete $ MDB.select otherQuery nodeCollectionName) (logVerbose "MongoDB deleted" . (:[]) . shows)
     testAction dbContext (MDB.insert nodeCollectionName otherDoc) (logVerbose "MongoDB inserted" . (:[]) . shows)
     testAllAction dbContext (MDB.find $ MDB.select otherQuery nodeCollectionName) (logVerbose "MongoDB nodes" . map shows)
     testAction dbContext (MDB.delete $ MDB.select otherQuery nodeCollectionName) (logVerbose "MongoDB deleted" . (:[]) . shows)
     return ()

-- EOF