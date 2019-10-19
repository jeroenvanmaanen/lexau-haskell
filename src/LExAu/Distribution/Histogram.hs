{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}
-- |Standard implementation of finite distributions.
module LExAu.Distribution.Histogram (
  histogramFromList,
  test
  ) where

import Data.IntMap (IntMap, Key, alter, fromList, fold, keys, toList)
import qualified Data.IntMap as IntMap (elems, findWithDefault, map, member)
import Data.Ratio (Ratio,numerator,denominator,(%))
import Numeric (showInt)
import System.Random (StdGen, getStdGen, randomR)

import LExAu.API.Alphabet (Alphabet)
import LExAu.API.DescriptionLength (DescriptionLength(descriptionLength))
import LExAu.API.Distribution (
    BaseDistribution,
    BaseHistogram(addHistogram,intCount,intIncrease,scale,subtractHistogram,weight),
    Distribution(hasMember,nextMember,probability),
    DistributionFactory(DistributionFactory),
    DistributionType(DistributionType),
    Histogram(count,increase,increment),
    IndexedHistogram(),
    IntDistribution(allInts,intProbability,hasInt,nextInt),
    LogDistribution(logDistribution),
    LogHistogram(logHistogram),
    randomSequence,
  )
import LExAu.API.Indexed (
    Collection(allMembers),
    Indexed(allIndices,maybeMember,member),
    IndexedMember(index)
  )
import LExAu.API.Named (Named, name)
import LExAu.API.ReaderContext (ReaderContext(readsWithContext))
import LExAu.Alphabet.Standard (createAlphabet)
import LExAu.Utilities.Data (flodder, select)
import LExAu.Utilities.DescriptionLength ()
import LExAu.Utilities.Logging (Loggable(logs),WrapShowS(WrapShowS),logVerbose)

type IntMapInteger = IntMap Integer

data WrappedIntList = WIL ([Int]) deriving (Eq, Show, Read)
data WrappedInt = WI (Int) deriving (Eq, Show, Read)
instance Collection WrappedIntList WrappedInt where
  allMembers (WIL members) = map WI members
instance IndexedMember WrappedInt Int where
  index (WI idx) = idx
instance Indexed WrappedIntList WrappedInt Int where
  member _ idx = WI idx
  maybeMember _ idx = Just $ WI idx
  allIndices (WIL members) = members
instance Loggable WrappedInt where
  logs (WI idx) = shows idx
createWrappedIntList :: [Int] -> WrappedIntList
createWrappedIntList intList = WIL intList

data DistributionImpl =
  FD { fdMapping :: !IntMapInteger
     , fdWeight :: !Integer
     } deriving (Eq, Show, Read)

type DistributionImplType = DistributionType DistributionImpl

instance Loggable DistributionImplType where
  logs distribution@(DistributionType (FD mapping _)) =
    logDistribution (createWrappedIntList (keys mapping)) distribution

instance BaseDistribution DistributionImpl where

instance BaseHistogram DistributionImpl where
  intCount (DistributionType (FD m _)) i = IntMap.findWithDefault 0 i m
  intIncrease (DistributionType (FD mapping weight)) key amount =
    let updater = increaseMaybe amount
        newMapping = alter updater key mapping :: IntMap Integer
        newWeight = weight + amount
        result = DistributionType $ FD newMapping newWeight
    in if signum weight * (signum $ IntMap.findWithDefault 0 key newMapping) < 0
         then error $ "Inconsistent sign: " ++ (show result)
         else result
  weight (DistributionType (FD _ w)) = w
  scale factor (DistributionType (FD m w)) =
    let newMapping = IntMap.map (scaleInteger factor) m
        newWeight = fold (+) 0 newMapping
    in DistributionType $ FD newMapping newWeight
  addHistogram this other =
    let keys = allInts other
    in flodder (\ i h -> intIncrease h i (intCount other i)) this (allInts other)
  subtractHistogram this other =
    let keys = allInts other
    in flodder (\ i h -> intIncrease h i (negate $ intCount other i)) this (allInts other)

scaleInteger :: Ratio Integer -> Integer -> Integer
scaleInteger factor number
  | factor < 0 = error "Scale factor should be nonnegative"
  | number < 0 = error "Number should be nonnegative"
  | factor == 0 || number == 0 = 0
  | True =
      let num = numerator factor
          denom = denominator factor
      in (((number * num) - 1) `div` denom) + 1

instance (
      Indexed collectionType memberType Int,
      IntDistribution distributionType
    ) => Distribution collectionType memberType distributionType where
  hasMember distribution symbol = hasInt distribution (index symbol :: Int)
  nextMember alphabet distribution stdGen =
    let (idx, nextGen) = nextInt distribution stdGen
        symbol = alphabet `member` idx
    in (symbol, nextGen)
  probability distribution symbol = intProbability distribution (index symbol :: Int)

instance IntDistribution DistributionImpl where
  hasInt (DistributionType (FD mapping _)) member = member `IntMap.member` mapping
  nextInt (DistributionType (FD mapping w)) theGen =
    let (dial, newGen) = randomR (1, w) theGen
        member = select (toList mapping) (fromInteger dial)
    in (member, newGen)
  intProbability (DistributionType (FD mapping weight)) key = (IntMap.findWithDefault 0 key mapping) % weight
  allInts (DistributionType (FD mappings _)) = keys mappings

instance (Histogram collectionType memberType DistributionImpl, Loggable memberType) => LogHistogram collectionType memberType DistributionImpl where

instance (LogHistogram collectionType memberType DistributionImpl, Loggable memberType) => LogDistribution collectionType memberType DistributionImpl where
  logDistribution collection distribution =
    logHistogram collection distribution

instance (Distribution collectionType memberType DistributionImpl) => Histogram collectionType memberType DistributionImpl where
  count (DistributionType (FD mapping weight)) symbol = IntMap.findWithDefault 0 (index symbol :: Int) mapping
  increase (DistributionType (FD mapping weight)) symbol amount =
    let key = (index symbol :: Int)
        updater = increaseMaybe amount
        newMapping = alter updater key mapping :: IntMap Integer
        newWeight = weight + amount
    in DistributionType $ FD newMapping newWeight

instance IndexedHistogram DistributionImpl where

instance DescriptionLength DistributionImpl where
  descriptionLength (FD mapping _) =
    foldl (+) 0 $ map descriptionLength $ IntMap.elems mapping

-- |Creates a new finite probability distribution defined by the given
-- (key,weight) assignments.
--
-- Such a distribution kan be used together with an Indexed collection to
-- represent a distribution over a colection of indexed members.
histogramFromList :: [(Key, Integer)] -> DistributionType DistributionImpl
histogramFromList list =
  let mapping = fromList list
      weight = fold (+) 0 mapping
  in DistributionType (FD mapping weight)

weight :: DistributionType DistributionImpl -> Integer
weight (DistributionType (FD mapping weight)) = weight

increaseMaybe :: Integer -> (Maybe Integer) -> (Maybe Integer)
increaseMaybe 0 maybeValue = maybeValue
increaseMaybe amount Nothing = Just amount
increaseMaybe amount (Just value) =
  let newValue = value + amount
  in if newValue == 0
       then Nothing
       else Just newValue

test :: IO ()
test = do
  let alphabet = createAlphabet "plankje" ["aap", "noot", "mies"]
      mies = alphabet `member` "mies"
      testDistribution = histogramFromList [(1,12),(2,7),(3,11)]
      copyOfTestDistribution = readsWithContext (DistributionFactory $ Just histogramFromList) $ show testDistribution
  putStrLn ""
  putStrLn $ "Test LExAu.Distribution.Histogram"
  print $ intProbability testDistribution 3
  print $ probability testDistribution mies
  logVerbose "Logs test distribution" [logs testDistribution]
  logVerbose "Log distribution test distribution" [logDistribution alphabet testDistribution]
  logVerbose "Shows test distribution" [shows testDistribution]
  logVerbose "Shows copy of test distribution" [shows copyOfTestDistribution]
  stdGen <- getStdGen
  (randomWords, nextGen) <- return $ randomSequence alphabet testDistribution stdGen 15
  print $ map name randomWords

-- EOF
