{-# LANGUAGE MultiParamTypeClasses #-}
module LExAu.Model.HistoryTreeImpl.ExpectedData
  ( ExpectedExtraProperties(..)
  , HistoryTreeExpectedImpl(..)
  , ReadExpectedContext(..)
  , addInfo
  , nodeDescriptionLength
  ) where

import LExAu.API.Distribution
  ( DistributionFactory(DistributionFactory)
  )
import LExAu.API.MDL (OptimizerType)
import LExAu.API.ReaderContext (ReaderContext(readsWithContext))
import LExAu.Model.HistoryTreeImpl.HTData (HTData(theExtraProperties))
import LExAu.Model.HistoryTreeImpl.Observed
  ( HistoryTreeModelImpl
  , ReadModelContext(..)
  )

data ExpectedExtraProperties =
  ExpectedExtraProperties
    { theDescriptionLength :: !Integer
    , theSolidChildrenCount :: !Int
    , theDistributionDirty :: !Bool
    , theInfo :: String
    , theParentKeyIndices :: Maybe [Int]
    }

instance Show ExpectedExtraProperties where
  showsPrec _ (ExpectedExtraProperties dl scc dd info _) = shows (dl, scc, dd, info)

instance Read ExpectedExtraProperties where
  readsPrec p s =
    let tuples :: [((Integer, Int, Bool, String), String)]
        tuples = readsPrec p s
        toExpectedExtraProperties :: (Integer, Int, Bool, String) -> ExpectedExtraProperties
        toExpectedExtraProperties (dl, scc, dd, info) = ExpectedExtraProperties dl scc dd info Nothing
    in map (\(r, s) -> (toExpectedExtraProperties r, s)) tuples

data HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType =
  HistoryTreeExpectedImpl
    { theObservedModel :: !(HistoryTreeModelImpl alphabetType symbolType observedDistributionType)
    , theCoveredData :: !(HTData observedDistributionType ())
    , theExpectedData :: !(HTData expectedDistributionType ExpectedExtraProperties)
    , theOptimizerState :: !(OptimizerType observedDistributionType expectedDistributionType oracleType)
    } deriving (Show, Read)

data ReadExpectedContext alphabetType symbolType observedDistributionType expectedDistributionType oracleType =
  ReadExpectedContext
    (ReadModelContext alphabetType symbolType observedDistributionType)
    (OptimizerType observedDistributionType expectedDistributionType oracleType)

-- |Method to reconstruct a history tree process from string representation and given the necessary factories.
instance
      ( Read alphabetType
      , Read symbolType
      , Read observedDistributionType
      , Read expectedDistributionType
      , Read oracleType
      ) =>
    ReaderContext (ReadExpectedContext alphabetType symbolType observedDistributionType expectedDistributionType oracleType) (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) where
  readsWithContext (ReadExpectedContext (ReadModelContext _ _ histogramFactory) _) s =
    map (\(r, s) -> (deriveParentKeyIndices r, s)) (reads s)

nodeDescriptionLength = theDescriptionLength . theExtraProperties

addInfo :: ExpectedExtraProperties -> ShowS -> ExpectedExtraProperties
addInfo oldProperties moreInfo =
  let oldInfo = theInfo oldProperties
      separator = if length oldInfo > 0 then " # " else ""
      newInfo = (
          showString oldInfo .
          showString separator .
          showChar '[' .
          moreInfo
        ) "]"
  in if False
       then oldProperties { theInfo = newInfo }
       else oldProperties

deriveParentKeyIndices :: (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) -> (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType)
deriveParentKeyIndices expected = 
  case deriveParentKeyIndicesHTData Nothing $ theExpectedData expected of
    Just newExpectedData -> expected { theExpectedData = newExpectedData }
    _ -> expected

deriveParentKeyIndicesHTData :: Maybe [Int] -> (HTData expectedDistributionType ExpectedExtraProperties) -> Maybe (HTData expectedDistributionType ExpectedExtraProperties)
deriveParentKeyIndicesHTData parentKeyIndices htData = Nothing

--EOF
