{-# LANGUAGE MultiParamTypeClasses #-}
module LExAu.API.Model
  ( AlphabetUpdate(..)
  , EvaluateUpdate(..)
  , HasCheckSum(checkSum)
  , ModelOfExpectedBehavior
  , OptimizeUpdate(..)
  , PolicyUpdate(..)
  , Updater(update)
  , UpdaterIO(updateIO)
  , pureUpdateToIO
  ) where

data AlphabetUpdate = AlphabetUpdate
data EvaluateUpdate = EvaluateUpdate
data OptimizeUpdate = OptimizeUpdate
data PolicyUpdate = PolicyUpdate

class Updater updateType modelType sourceType where
  update :: updateType -> sourceType -> modelType -> Maybe modelType

class UpdaterIO updateType modelType sourceType where
  updateIO :: updateType -> sourceType -> modelType -> IO (Maybe modelType)

pureUpdateToIO :: (updateType -> sourceType -> modelType -> (Maybe modelType)) -> updateType -> sourceType -> modelType -> IO (Maybe modelType)
pureUpdateToIO updateFunction kind update model = return $ updateFunction kind update model

class ModelOfExpectedBehavior expectedType where

class HasCheckSum modelType where
  checkSum :: modelType -> Integer

-- EOF
