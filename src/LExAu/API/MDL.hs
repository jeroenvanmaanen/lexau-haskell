{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module LExAu.API.MDL (
    Optimizer(optimize,updateOptimizer,updateOptimizerIO),
    OptimizerType(),
    createOptimizer,
    createOptimizerIO,
    optimizerOracle
  ) where

import Control.Monad (liftM)
import Data.Monoid (Monoid(mappend))

import LExAu.API.Distribution (
    BaseDistribution(),
    BaseHistogram(),
    DistributionType(DistributionType),
    DistributionFactory()
  )

data OptimizerMethods observedDistributionType expectedDistributionType oracleType =
  OptimizerMethods
    { maybeOracleUpdater :: Maybe (Integer -> oracleType -> Maybe oracleType)
    , maybeOracleUpdaterIO :: Maybe (Integer -> oracleType -> IO (Maybe oracleType))
    , optimizer :: DistributionType observedDistributionType -> oracleType -> DistributionType expectedDistributionType
    }

data OptimizerType observedDistributionType expectedDistributionType oracleType =
  OptimizerImpl
    { optimizerMethods :: Maybe (OptimizerMethods observedDistributionType expectedDistributionType oracleType)
    , optimizerOracle :: oracleType
    }

class Optimizer optimizerType observedDistributionType expectedDistributionType | optimizerType -> observedDistributionType, optimizerType -> expectedDistributionType where
  optimize :: optimizerType -> DistributionType observedDistributionType -> DistributionType expectedDistributionType
  updateOptimizer :: Integer -> optimizerType -> Maybe optimizerType
  updateOptimizerIO :: Integer -> optimizerType -> IO (Maybe optimizerType)

updateOptimizerToIO :: (Integer -> optimizerType -> Maybe optimizerType) -> Integer -> optimizerType -> IO (Maybe optimizerType)
updateOptimizerToIO f w u = return $ f w u

instance Optimizer (OptimizerType observedDistributionType expectedDistributionType oracleType) observedDistributionType expectedDistributionType where
  updateOptimizer w oldOptimizerImpl =
    do let oldOracle = optimizerOracle oldOptimizerImpl
       methods <- optimizerMethods oldOptimizerImpl
       updater <- maybeOracleUpdater methods
       newOracle <- updater w oldOracle
       return $ oldOptimizerImpl { optimizerOracle = newOracle}

  updateOptimizerIO w oldOptimizerImpl =
    let oldOracle = optimizerOracle oldOptimizerImpl
        maybeUpdaterIO = -- Maybe (Integer -> optimizerType -> IO (Maybe optimizerType))
          do -- Maybe monad
             methods <- optimizerMethods oldOptimizerImpl
             case maybeOracleUpdaterIO methods of
               theOracleUpdaterIO@(Just _) -> theOracleUpdaterIO
               Nothing -> updateOptimizerToIO `liftM` (maybeOracleUpdater methods) 
    in do -- IO monad
          maybeNewOracle <-
            case maybeUpdaterIO of
              Just updaterIO ->
                do updaterIO w oldOracle
              Nothing -> return Nothing
          return $
            case maybeNewOracle of
              Just newOracle -> Just $ oldOptimizerImpl { optimizerOracle = newOracle }
              Nothing -> Nothing

  optimize optimizerImpl observedDistribution =
    case optimizerMethods optimizerImpl of
      Just methods ->
        let optimizerFunction = optimizer methods
            theOracle = optimizerOracle optimizerImpl
        in optimizerFunction observedDistribution theOracle

instance (Show oracleType) => Show (OptimizerType observedDistributionType expectedDistributionType oracleType) where
  showsPrec level optimizer = showsPrec level $ optimizerOracle optimizer

instance (Read oracleType) => Read (OptimizerType observedDistributionType expectedDistributionType oracleType) where
  readsPrec level s = map (\(oracle, t) -> ((OptimizerImpl Nothing oracle), t)) (readsPrec level s)

createOptimizer ::
    oracleType -- initialOracle
      -> (Integer -> oracleType -> Maybe oracleType) -- updater
      -> (DistributionFactory observedDistributionType) -- only the type is used to fix the type of the result
      -> (DistributionType observedDistributionType -> oracleType -> DistributionType expectedDistributionType) -- optimizer function
      -> (OptimizerType observedDistributionType expectedDistributionType oracleType)
createOptimizer initialOracle updater observedDistributionFactory optimizerFunction =
  OptimizerImpl (Just $ OptimizerMethods (Just updater) Nothing optimizerFunction) initialOracle

createOptimizerIO ::
    oracleType -- initialOracle
      -> (Integer -> oracleType -> IO (Maybe oracleType)) -- updater
      -> (DistributionFactory observedDistributionType) -- only the type is used to fix the type of the result
      -> (DistributionType observedDistributionType -> oracleType -> DistributionType expectedDistributionType) -- optimizer function
      -> (OptimizerType observedDistributionType expectedDistributionType oracleType)
createOptimizerIO initialOracle updaterIO observedDistributionFactory optimizerFunction =
  OptimizerImpl (Just $ OptimizerMethods Nothing (Just updaterIO) optimizerFunction) initialOracle

-- EOF
