{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
-- |Interface module for finite probability distributions.
--
-- Use a factory method in an implementation module of this interface to create
-- instances (e.g., LExAu.Distribution.Histogram.histogramFromList).
module LExAu.API.Distribution (
    BaseDistribution,
    BaseHistogram(addHistogram,intCount,intIncrease,maybeSubtractHistogram,scale,subtractHistogram,weight),
    Distribution(hasMember,nextMember,probability),
    DistributionFactory(DistributionFactory,create),
    DistributionType(DistributionType),
    Histogram(count,increase,increment),
    IndexedHistogram(),
    IntDistribution(allInts,intProbability,hasInt,nextInt),
    LogDistribution(logDistribution),
    LogHistogram(logHistogram),
    NegatableHistogram,
    flipHistogram,
    formatProbability,
    negateHistogram,
    randomSequence,
    straightHistogram
  ) where

import Data.Ratio (Ratio, (%))
import Numeric (showInt)
import System.Random (StdGen)
import Text.Show (showListWith)
import qualified Data.Set as Set (Set, difference, fromList, toList) 

import LExAu.API.Indexed (Indexed, allMembers, index)
import LExAu.API.DescriptionLength (
    DescriptionLength(descriptionLength),
    PrefixEncode(prefixEncode)
  )
import LExAu.API.ReaderContext (ReaderContext(readsWithContext))
import LExAu.Utilities.Logging (Loggable(logs))

-- |A container for a distribution object
data DistributionType distributionType = DistributionType distributionType deriving (Eq, Show, Read)

instance (DescriptionLength distributionType) => DescriptionLength (DistributionType distributionType) where
  descriptionLength (DistributionType distribution) = descriptionLength distribution

instance (PrefixEncode distributionType) => PrefixEncode (DistributionType distributionType) where
  prefixEncode (DistributionType distribution) = prefixEncode distribution

-- |Factory object that can be carried around and that can be used to create
-- new finite distributions when necessary.
data DistributionFactory distributionType = DistributionFactory { create :: Maybe ([(Int,Integer)] -> (DistributionType distributionType)) }

instance Show (DistributionFactory distributionType) where
  showsPrec _ factory = shows (Nothing :: Maybe ())

instance Read (DistributionFactory distributionType) where
  readsPrec _ s = map (\(_, t) -> (DistributionFactory Nothing, t)) $ (reads s :: [(Maybe (),String)])

-- |The type class of a distribution (independent of its domain).
class BaseDistribution distributionType where

-- |Method to reconstruct a distribution given a factory and a string representation.
instance (Read distributionType) => ReaderContext (DistributionFactory distributionType) (DistributionType distributionType) where
  readsWithContext histogramFactory s = (reads s) 

-- |Methods for a distribution (in combination with members of its domain).
class (
      Indexed collectionType memberType Int,
      BaseDistribution distributionType
    ) =>
    Distribution collectionType memberType distributionType
    where
  -- |Checks whether the given distribution defines a value for the given member.
  hasMember :: DistributionType distributionType -> memberType -> Bool

  -- |Draws a new member from the collection according to the given distribution.
  nextMember :: collectionType -> DistributionType distributionType -> StdGen -> (memberType, StdGen)

  -- |Returns the probability assigned by the given distribution to the given member.
  probability :: DistributionType distributionType -> memberType -> Rational

-- |Methods to log a distribution.
class (
      Distribution collectionType memberType distributionType,
      Loggable memberType,
      Show distributionType
    ) =>
    LogDistribution collectionType memberType distributionType where
  logDistribution :: collectionType -> DistributionType distributionType -> ShowS
  -- |Shows the given distribution (using the ShowS pattern).
  logDistribution collection distribution =
    showString "{Dist: " .
    showListWith (logEntry collection distribution)  (filter (hasMember distribution) (allMembers collection)) .
    showUnknownDistributionMembers collection distribution .
    showChar '}'
  showUnknownDistributionMembers :: collectionType -> (DistributionType distributionType) -> ShowS
  showUnknownDistributionMembers collection distribution = id

class (
      BaseDistribution distributionType
    ) =>
    IntDistribution distributionType
    where
  -- |Checks whether the given distribution defines a value for the given member.
  hasInt :: DistributionType distributionType -> Int -> Bool

  -- |Draws a new member from the collection according to the given distribution.
  nextInt :: DistributionType distributionType -> StdGen -> (Int, StdGen)

  -- |Returns the probability assigned by the given distribution to the given member.
  intProbability :: DistributionType distributionType -> Int -> Rational

  -- |Returns all integers in the domain of the given distribution.
  allInts :: DistributionType distributionType -> [Int]

-- |Methods for a histogram that backs a distribution (independent of its domain).
class BaseHistogram distributionType where
  -- |Returns the count value assigned to the given member in the given distribution.
  intCount :: DistributionType distributionType -> Int -> Integer

  -- |Returns a copy of the given distribution that has the count of the given member increased by the given amount.
  intIncrease :: DistributionType distributionType -> Int -> Integer -> DistributionType distributionType

  -- |Returns the sum of all the histogram counts in the given distribution.
  weight :: DistributionType distributionType -> Integer

  -- |Returns the histogram that is scaled (and rounded) with the given scale factor.
  scale :: Ratio Integer -> DistributionType distributionType -> DistributionType distributionType

  -- |Adds a histogram to the given histogram
  addHistogram :: (
        BaseHistogram otherHistogramType,
        IntDistribution otherHistogramType
      ) =>
    DistributionType distributionType -> DistributionType otherHistogramType -> DistributionType distributionType

  -- |Subtracts a histogram from the given histogram
  subtractHistogram :: (
        BaseHistogram otherHistogramType,
        IntDistribution otherHistogramType
      ) =>
    DistributionType distributionType -> DistributionType otherHistogramType -> DistributionType distributionType

  -- |Subtracts a histogram from the given histogram, maybe
  maybeSubtractHistogram :: (
        BaseHistogram otherHistogramType,
        IntDistribution otherHistogramType
      ) =>
    DistributionType distributionType -> Maybe (DistributionType otherHistogramType) -> DistributionType distributionType

  maybeSubtractHistogram this maybeOther =
    case maybeOther of
      Just other -> subtractHistogram this other
      Nothing -> this

data NegatableHistogram baseType = StraightHistogram baseType | NegatedHistogram baseType deriving Show

straightHistogram :: (BaseHistogram originalType) => DistributionType originalType -> DistributionType (NegatableHistogram originalType)
straightHistogram (DistributionType original) = DistributionType $ StraightHistogram original

negateHistogram :: (BaseHistogram originalType) => DistributionType originalType -> DistributionType (NegatableHistogram originalType)
negateHistogram (DistributionType original) = DistributionType $ NegatedHistogram original

flipHistogram :: DistributionType (NegatableHistogram originalType) -> DistributionType (NegatableHistogram originalType)
flipHistogram (DistributionType (StraightHistogram baseHistogram)) = DistributionType $ NegatedHistogram baseHistogram
flipHistogram (DistributionType (NegatedHistogram baseHistogram)) = DistributionType $ StraightHistogram baseHistogram

instance (BaseHistogram baseType) => BaseHistogram (NegatableHistogram baseType) where
  intCount (DistributionType negatableHistogram) =
    case negatableHistogram of
      (StraightHistogram baseHistogram) -> intCount $ DistributionType baseHistogram
      (NegatedHistogram baseHistogram) -> negate . (intCount $ DistributionType baseHistogram)
  intIncrease (DistributionType negatableHistogram) key =
    case negatableHistogram of
      (StraightHistogram baseHistogram) -> straightHistogram . (intIncrease (DistributionType baseHistogram) key)
      (NegatedHistogram baseHistogram) -> negateHistogram . (intIncrease (DistributionType baseHistogram) key) . negate
  weight (DistributionType negatableHistogram) =
    case negatableHistogram of
      (StraightHistogram baseHistogram) -> weight $ DistributionType baseHistogram
      (NegatedHistogram baseHistogram) -> negate $ weight $ DistributionType baseHistogram
  scale ratio (DistributionType negatableHistogram) =
    case negatableHistogram of
      (StraightHistogram baseHistogram) -> straightHistogram (scale ratio $ DistributionType baseHistogram)
      (NegatedHistogram baseHistogram) -> negateHistogram (scale ratio $ DistributionType baseHistogram)
  addHistogram (DistributionType negatableHistogram) =
    case negatableHistogram of
      (StraightHistogram baseHistogram) -> straightHistogram . (addHistogram $ DistributionType baseHistogram)
      (NegatedHistogram baseHistogram) -> negateHistogram . (subtractHistogram $ DistributionType baseHistogram)
  subtractHistogram (DistributionType negatableHistogram) =
    case negatableHistogram of
      (StraightHistogram baseHistogram) -> straightHistogram . (subtractHistogram $ DistributionType baseHistogram)
      (NegatedHistogram baseHistogram) -> negateHistogram . (addHistogram $ DistributionType baseHistogram)

instance (BaseDistribution baseType) => BaseDistribution (NegatableHistogram baseType)

instance (IntDistribution baseType) => IntDistribution (NegatableHistogram baseType) where
  hasInt (DistributionType negatableHistogram) =
    case negatableHistogram of
      (StraightHistogram baseHistogram) -> hasInt $ DistributionType baseHistogram
      (NegatedHistogram baseHistogram) -> hasInt $ DistributionType baseHistogram
  nextInt (DistributionType negatableHistogram) =
    case negatableHistogram of
      (StraightHistogram baseHistogram) -> nextInt $ DistributionType baseHistogram
      (NegatedHistogram baseHistogram) ->
	(\ (i, g) -> (negate i, g)) . (nextInt $ DistributionType baseHistogram)
  intProbability (DistributionType negatableHistogram) =
    case negatableHistogram of
      (StraightHistogram baseHistogram) -> intProbability $ DistributionType baseHistogram
  allInts (DistributionType negatableHistogram) =
    case negatableHistogram of
      (StraightHistogram baseHistogram) -> allInts $ DistributionType baseHistogram
      (NegatedHistogram baseHistogram) -> allInts $ DistributionType baseHistogram

-- |Methods for a histogram that backs a distribution (in combination with members of its domain).
class (
      Distribution collectionType memberType distributionType,
      BaseHistogram distributionType
    ) =>
    Histogram collectionType memberType distributionType
    where
  -- |Returns the count value assigned to the given member in the given distribution.
  count :: DistributionType distributionType -> memberType -> Integer

  -- |Returns a copy of the given distribution that has the count of the given member increased by the given amount.
  increase :: DistributionType distributionType -> memberType -> Integer -> DistributionType distributionType

  -- |Returns a copy of the given distribution that has the count of the given member increased by unity.
  increment :: DistributionType distributionType -> memberType -> DistributionType distributionType
  increment distribution member = increase distribution member 1

class (BaseHistogram distributionType, IntDistribution distributionType) => IndexedHistogram distributionType where

-- |Methods to log a histogram that backs a distribution.
class (
      Histogram collectionType memberType distributionType, Loggable memberType,
      IntDistribution distributionType
    ) =>
    LogHistogram collectionType memberType distributionType where
  -- |Shows the given distribution (using the ShowS pattern).
  logHistogram :: collectionType -> DistributionType distributionType -> ShowS
  logHistogram collection distribution =
    showString "{Hist: " .
    showListWith (logHistogramEntry collection distribution)  (filter (hasMember distribution) (allMembers collection)) .
    showUnknownHistogramMembers collection distribution .
    showString ", " .
    shows (weight distribution) .
    showChar '}'
  showUnknownHistogramMembers :: collectionType -> (DistributionType distributionType) -> ShowS
  showUnknownHistogramMembers collection distribution =
    let indices = Set.fromList $ allInts distribution
        knownIndices :: Set.Set Int
        knownIndices = Set.fromList $ map index $ allMembers collection
        unknownHistogramMembers = Set.toList $ indices `Set.difference` knownIndices
    in if (length unknownHistogramMembers) > 0
       then showString ", ??: " . showListWith (logNumericHistogramEntry distribution) unknownHistogramMembers
       else id

-- |Returns a sequence of randomly drawn members of the collection according to the given distribution.
randomSequence ::
  (Distribution collectionType memberType distributionType) =>
  collectionType -> DistributionType distributionType -> StdGen -> Integer -> ([memberType], StdGen)
randomSequence _ _ theGen 0 = ([], theGen)
randomSequence collection distribution theGen l =
  let (x, nextGen) = nextMember collection distribution theGen
      (xs, lastGen) = randomSequence collection distribution nextGen (l - 1)
    in ((x : xs), lastGen)

logEntry :: (LogDistribution collectionType memberType distributionType) =>
  collectionType -> DistributionType distributionType -> memberType -> ShowS
logEntry collection distribution member =
  logs member .
  showChar ':' .
  (formatProbability $ probability distribution member)

logHistogramEntry :: (LogHistogram collectionType memberType distributionType) =>
  collectionType -> DistributionType distributionType -> memberType -> ShowS
logHistogramEntry collection distribution member =
  logs member .
  showChar ':' .
  (shows $ count distribution member) .
  showChar '~' .
  (formatProbability $ probability distribution member)

logNumericHistogramEntry :: (BaseHistogram distributionType) =>
  DistributionType distributionType -> Int -> ShowS
logNumericHistogramEntry distribution idx =
  let count = intCount distribution idx
  in showInt idx .
     showChar ':' .
     (shows $ intCount distribution idx) .
     showChar '~' .
     (formatProbability $ count % (weight distribution))

formatProbability :: Ratio Integer -> ShowS
formatProbability r = showInt (ceiling (r * 100)) . ('%':)
