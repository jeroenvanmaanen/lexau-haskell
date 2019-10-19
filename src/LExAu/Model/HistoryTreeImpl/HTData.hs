{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module LExAu.Model.HistoryTreeImpl.HTData
  ( HTData(..)
  , HTStatus(..)
  , maybeStatusChar
  , statusChar
  ) where

import Data.IntMap (IntMap)
import Text.Show (showListWith)
import qualified Data.IntMap as IntMap (assocs, lookup, null)

import LExAu.API.Distribution
  ( DistributionType
  , LogDistribution(logDistribution)
  )
import LExAu.API.HistoryTree (UpdateMarkers(UpdateMarkers))
import LExAu.API.Indexed (member)
import LExAu.API.Named (Named(name))
import LExAu.Model.HistoryTreeImpl.Utilities
  ( LogEntry(logEntry)
  , modifiedChar
  )

data HTStatus = Barrier | Extensible | Virtual | Solid deriving (Eq, Show, Read)

data HTData distributionType extraPropertiesType =
  HTData
    { theDistribution :: !(DistributionType distributionType)
    , theChildren :: !(IntMap (HTData distributionType extraPropertiesType))
    , theStatus :: !HTStatus
    , theExtraProperties :: !extraPropertiesType
    } deriving (Show, Read)

instance (
        LogDistribution alphabetType symbolType distributionType,
        Named symbolType
      ) =>
    LogEntry (HTData distributionType extraPropertiesType) alphabetType symbolType where
  logEntry symbols maybeUpdated indent (keyIndex, entry) =
    let subupdated =
          case maybeUpdated of
            Just (UpdateMarkers updatedMap) -> keyIndex `IntMap.lookup` updatedMap
            Nothing -> Nothing
        subindent = indent ++ "   "
        childNodes = theChildren entry
        distribution = theDistribution entry
    in showString "\n" .
       showString indent .
       showChar (modifiedChar subupdated) .
       showChar (statusChar entry) .
       showChar ' ' .
       ( if keyIndex >= 0
           then shows (name (symbols `member` keyIndex)) .
                showChar ':'
           else id
       ) .
       logDistribution symbols distribution .
       if IntMap.null childNodes
         then showString " [no-children] "
         else
           showString "\n" . showString indent . showString "    " .
           (showListWith (logEntry symbols subupdated subindent) $ IntMap.assocs childNodes)

maybeStatusChar :: (Maybe (HTData distributionType extraPropertiesType)) -> Char
maybeStatusChar maybeModelData =
  case maybeModelData of
    Nothing -> '0'
    Just modelData ->
      case theStatus modelData of
        Extensible -> '>'
        Barrier -> 'X'
        Virtual -> '-'
        Solid -> '+'

statusChar :: (HTData distributionType extraPropertiesType) -> Char
statusChar modelData = maybeStatusChar $ Just modelData

--EOF
