{-# LANGUAGE MultiParamTypeClasses #-}

module LExAu.API.Database
  ( BaseWorker(..)
  , InstanceKind(..)
  , MarshallWorker(..)
  , Worker(..)
  )
  where

import Data.Time.Clock (UTCTime)

data InstanceKind = Recorder | Optimizer | Evaluator | Consolidator
data WorkerKey oidType =
  WorkerKey
    { workerKeyInstanceKind :: InstanceKind
    , workerKeyId :: oidType
    }

class BaseWorker workerType oidType where
  getWorkerKey :: workerType -> WorkerKey oidType
  getStartTime :: workerType -> UTCTime
  getPingTime :: workerType -> UTCTime

class MarshallWorker workerType recordType where
  marshallWorker :: workerType -> recordType
  unmarshallWorker :: InstanceKind -> recordType -> workerType

class ( BaseWorker workerType oidType
      , MarshallWorker workerType recordType
      ) =>
    Worker workerType oidType recordType dbContextType where
  loadWorker :: dbContextType -> WorkerKey oidType -> IO (Maybe workerType)
  saveWorker :: dbContextType -> workerType -> IO ()

class BaseBucket bucketType oidType where
  getBucketId :: bucketType -> oidType
  getCreationTime :: bucketType -> UTCTime
  getMergedBucketIds :: bucketType -> [oidType]
  getOwnerKey :: bucketType -> WorkerKey oidType
  getActiveFlag :: bucketType -> Bool

class MarshallBucket bucketType recordType where
  marshallBucket :: bucketType -> recordType
  unmarshallBucket :: recordType -> bucketType

class ( BaseBucket bucketType oidType
      , MarshallBucket bucketType recordType
      ) =>
    Bucket bucketType oidType recordType dbContextType where
  loadBucket :: dbContextType -> oidType -> IO (Maybe bucketType)
  saveBucket :: dbContextType -> bucketType -> IO ()

--EOF
