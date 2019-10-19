{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module LExAu.Distribution.MDL (createMDLOptimizer, createMDLOptimizerFromCache, precompute, test) where

import Control.Exception (bracket)
import Control.Monad (liftM)
import Data.List (foldl', foldl1', sort, sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Ratio ((%))
import System.IO
  ( Handle
  , IOMode(ReadMode, ReadWriteMode)
  , SeekMode(AbsoluteSeek)
  , hClose
  , hFlush
  , hGetLine
  , hIsEOF
  , hPutStr
  , hPutStrLn
  , hSeek
  , hTell
  , openFile
  , stdout
  )
import System.Random (getStdGen,randomR)
import LExAu.API.DescriptionLength
  ( DescriptionLength(descriptionLength)
  , PrefixEncode(prefixEncode)
  , toTerseCode
  )
import LExAu.API.Distribution (
    BaseDistribution(),
    BaseHistogram(weight),
    Distribution(probability),
    DistributionFactory(DistributionFactory),
    DistributionType(DistributionType),
    IntDistribution(allInts,intProbability,hasInt,nextInt),
    LogDistribution(logDistribution),
    randomSequence
  )
import LExAu.API.Indexed(Indexed(member))
import LExAu.API.MDL
  ( Optimizer(updateOptimizerIO)
  , OptimizerType
  , createOptimizer
  , createOptimizerIO
  , optimizerOracle
  )
import LExAu.API.Named (Named(name))
import LExAu.Alphabet.Standard (createAlphabet)
import LExAu.Distribution.Histogram (histogramFromList)
import LExAu.Utilities.Data (flodder,select)
import LExAu.Utilities.DescriptionLength (binaryEncodeInteger,bitLength)
import LExAu.Utilities.Logging (Loggable(logs),Summarize(summarize),logVerbose,showListVertically)
import qualified Data.IntMap as IntMap (IntMap, empty, findWithDefault, fold, foldWithKey, fromList, keys, member, size, toList)

data MDLDistributionImpl = MDLDistributionImpl { mdlMappings :: IntMap.IntMap (Integer, Rational) } deriving (Eq, Show, Read)

instance BaseDistribution MDLDistributionImpl where

instance IntDistribution MDLDistributionImpl where
  hasInt (DistributionType (MDLDistributionImpl mapping)) i = i `IntMap.member` mapping

  nextInt (DistributionType (MDLDistributionImpl mapping)) theGen =
    let dial :: Double
        (dial, newGen) = randomR (0.0, 1.0) theGen
        probabilities = map (\(i, (n, r)) -> (i, r)) (IntMap.toList mapping)
        member = select (probabilities) (toRational dial)
    in (member, newGen)

  intProbability (DistributionType (MDLDistributionImpl mapping)) i =
    snd $ IntMap.findWithDefault (0, 0 % 1) i mapping

  allInts (DistributionType (MDLDistributionImpl mapping)) = IntMap.keys mapping

instance DescriptionLength MDLDistributionImpl where
  descriptionLength distribution =
    let mappings = mdlMappings distribution
    in IntMap.foldWithKey addPairDescriptionLength (descriptionLength $ toInteger $ IntMap.size mappings) mappings

addPairDescriptionLength:: Int -> (Integer, Rational) -> Integer -> Integer
addPairDescriptionLength key (idx, fraction) accumulator =
  accumulator + (sum [
      descriptionLength $ toInteger key,
      descriptionLength idx
    ])

instance DescriptionLength (Maybe [Int], DistributionType MDLDistributionImpl) where
  descriptionLength (maybeParentKeyIndices, DistributionType distribution) =
    case maybeParentKeyIndices of
      Just parentKeyIndices -> -- toInteger $ length $ toTerseCode $ prefixEncode (parentKeyIndices, distribution) ""
        let mappings = mdlMappings distribution
            parentEntryCount = toInteger $ length parentKeyIndices
            entryCount = toInteger $ IntMap.size mappings
            droppedEntries = parentEntryCount - entryCount
            (sparseIdxSize, sparseKeySize) =
              if parentEntryCount <= 1
                then (1, (descriptionLength (0 :: Integer)) + 1)
                else
                  let theSparseIdxSize = bitLength $ toInteger $ parentEntryCount - 1
                  in (theSparseIdxSize, entryCount * theSparseIdxSize)
            sparseEntryCountSize = descriptionLength entryCount
            denseDroppedSize = droppedEntries * (descriptionLength (0 :: Integer))
        in if denseDroppedSize <= (sparseEntryCountSize + sparseKeySize)
             then flodder
                    (\ i n -> n + descriptionLength (fst (IntMap.findWithDefault (0, 0%1) i mappings)))
                    1 -- Flag to indicate dense encoding
                    parentKeyIndices
             else
               let idxPadding = 2 ^ sparseIdxSize
               in flodder
                    (addSparseCodeLength mappings sparseIdxSize)
                    ( 1 -- Flag to indicate sparse encoding
                    + descriptionLength sparseEntryCountSize
                    )
                    parentKeyIndices
      _ -> descriptionLength distribution

addSparseCodeLength :: (IntMap.IntMap (Integer, Rational)) -> Integer -> Int -> Integer -> Integer
addSparseCodeLength mappings keyIdxCodeLength key n =
  let (valueIdx, value) = (IntMap.findWithDefault (0, 0%1) key mappings)
  in if value > 0%1
       then n + keyIdxCodeLength + descriptionLength valueIdx
       else n

instance PrefixEncode MDLDistributionImpl where
  prefixEncode distribution =
    let mappings = mdlMappings distribution
        sizeCode = showString " #" . (prefixEncode $ IntMap.size mappings) . showString ": "
    in showChar '[' .
       IntMap.foldWithKey addPairEncoding sizeCode mappings .
       showChar ']'

addPairEncoding :: Int -> (Integer, Rational) -> ShowS -> ShowS
addPairEncoding key (idx, fraction) buffer =
  buffer .
  prefixEncode key .
  showString " '->' " .
  prefixEncode idx .
  showChar ','

instance PrefixEncode (Maybe [Int], DistributionType MDLDistributionImpl) where
  prefixEncode (parentKeys, DistributionType distribution) = prefixEncode (parentKeys, distribution)

instance PrefixEncode (Maybe [Int], MDLDistributionImpl) where
  prefixEncode (maybeParentKeys, distribution) =
    case maybeParentKeys of
      Just parentKeys ->
            let mappings = mdlMappings distribution
                parentEntryCount = toInteger $ length parentKeys
                entryCount = toInteger $ IntMap.size mappings
                droppedEntries = parentEntryCount - entryCount
                (sparseIdxSize, sparseKeySize) =
                  if parentEntryCount <= 1
                    then (1, (descriptionLength (0 :: Integer)) + 1)
                    else
                      let theSparseIdxSize = bitLength $ toInteger $ parentEntryCount - 1
                      in (theSparseIdxSize, entryCount * theSparseIdxSize)
                denseDroppedSize = droppedEntries * (descriptionLength (0 :: Integer))
                code =
                  if denseDroppedSize <= sparseKeySize
                    then flodder
                           (addSingleEncoding mappings)
                           (showString "O(dense): [")
                           parentKeys .
                         showString " ]"
                    else
                      let idxPadding = 2 ^ sparseIdxSize
                      in flodder
                           (addSparseEncoding mappings idxPadding)
                           (showString "I(sparse)" . prefixEncode entryCount . showString ": [ ")
                           (zip [0..] parentKeys) .
                         showString " ]"
            in showChar '[' .
               code .
               showChar ']'
      _ -> showChar '[' . (prefixEncode distribution) . showChar ']'

addSparseEncoding :: (IntMap.IntMap (Integer, Rational)) -> Int -> (Int, Int) -> ShowS -> ShowS
addSparseEncoding mappings idxPadding (idx, key) buffer =
  let (valueIndex, value) = IntMap.findWithDefault (0, 0%1) key mappings
  in if value > 0%1
       then
         let idxNumber = idxPadding + idx
             (one : idxCode) = binaryEncodeInteger $ toInteger idxNumber
         in buffer .
            showString idxCode .
            showString "~(" .
            shows key .
            showString ") '->' " .
            prefixEncode valueIndex .
            showString "~(" .
            shows value .
            showString "),"
       else buffer

addSingleEncoding :: (IntMap.IntMap (Integer, Rational)) -> Int -> ShowS -> ShowS
addSingleEncoding mappings key buffer =
  let (valueIndex, value) = IntMap.findWithDefault (0, 0%1) key mappings
  in buffer .
     showString "#(" .
     shows key .
     showString ") '->' " .
     if value > 0%1
       then prefixEncode valueIndex .
            showString "~(" .
            shows value .
            showString "),"
       else prefixEncode (0 :: Integer) . showString "~(0%1),"

instance (
      Indexed collectionType memberType Int,
      Loggable memberType
    ) =>
    LogDistribution collectionType memberType MDLDistributionImpl where

instance Loggable (OptimizerType observedDistributionType expectedDistributionType RationalSegments) where
  logs = summarize

instance Summarize (OptimizerType observedDistributionType expectedDistributionType RationalSegments) where
  summarize optimizerState =
    let ((w, ((n, r) : _)) : _) = theSegmentList $ optimizerOracle optimizerState
    in showString "Oracle summary: " . showsPrec 0 (w, n, r)

-- |Optimizes the model of expected behavior based on the model of observed behavior.
optimize :: (
      BaseHistogram observedDistributionType,
      IntDistribution observedDistributionType
    ) =>
    DistributionType observedDistributionType -> RationalSegments -> DistributionType MDLDistributionImpl

optimize distribution rationalSegments =
  let theWeight = weight distribution
      expectedDistributionList =
        if theWeight > 0
          then
            let unsortedRationals = getRationalPairsFromSegments rationalSegments theWeight
                rationals = sortBy (\(_, r1) (_, r2) -> r1 `compare` r2) unsortedRationals
                pairs = distributionToPairs distribution
                selected = selectRationals pairs rationals
                deltaList = triplesToDistributionList $ reverse selected
                sortedDeltaList = tail $ sortBy (\(n1, r1) (n2, r2) -> r1 `compare` r2) deltaList
                zipped = zip pairs sortedDeltaList
            in map (\((i, _), (n, r)) -> (i, (n, r))) zipped
          else []
  in DistributionType $ MDLDistributionImpl $ IntMap.fromList expectedDistributionList

createMDLOptimizer :: (
      BaseHistogram observedDistributionType,
      IntDistribution observedDistributionType
    ) => DistributionFactory observedDistributionType -> OptimizerType observedDistributionType MDLDistributionImpl RationalSegments

createMDLOptimizer distributionFactory =
  let Just initialOracle = updateRationalSegmentList [] 1
  in createOptimizer (RationalSegments 0 initialOracle) updateRationalSegments distributionFactory optimize

distributionToPairs :: (
      IntDistribution distributionType
    ) =>
  DistributionType distributionType -> [(Int, Rational)]

distributionToPairs distribution =
  let simplePairs = map (\i -> (i, intProbability distribution i)) (allInts distribution)
      filteredPairs = filter (\(i, r) -> r > 0) simplePairs
  in if (length filteredPairs) < 2
       then filteredPairs
       else let sortedPairs = sortBy (\(_, r1) (_, r2) -> r2 `compare` r1) filteredPairs
                accumulatedPairs = foldr (\(i1, r1) pairs@((i2, r2) : _) -> (i1, r1 + r2) : pairs) [(0, 0)] sortedPairs
            in tail $ reverse accumulatedPairs

selectRationals :: [(Int, Rational)] -> [(Integer, Rational)] -> [(Int, Integer, Rational)]

selectRationals distributionPairs rationals =
  let domainSize = toInteger $ length distributionPairs
  in if domainSize < 2
       then selectRationalsRecursive distributionPairs rationals
       else let extra = map (\i -> (i, i % domainSize)) [1..domainSize - 1]
                sortedRationals = sortBy (\(_, r1) (_, r2) -> r1 `compare` r2) (extra ++ rationals)
            in selectRationalsRecursive distributionPairs sortedRationals

selectRationalsRecursive :: [(Int, Rational)] -> [(Integer, Rational)] -> [(Int, Integer, Rational)]

selectRationalsRecursive distributionPairs@((fdi, fdr) : otherDistributionPairs) ((ffi, ffr) : otherRationals) =
  if fdr <= ffr
    then (fdi, ffi, ffr) : (selectRationalsRecursive otherDistributionPairs otherRationals)
    else selectRationalsRecursive distributionPairs otherRationals

selectRationalsRecursive [] _ = []

selectRationalsRecursive distribution rationals = error $ "Can't select rationals for: " ++ (show distribution) ++ ": " ++ (show rationals)

triplesToDistributionList :: [(Int, Integer, Rational)] -> [(Integer, Rational)]

triplesToDistributionList triples = snd $ foldr addTripleToDistributionList (0, [(0, 0)]) triples

addTripleToDistributionList :: (Int, Integer, Rational) -> (Rational, [(Integer, Rational)]) -> (Rational, [(Integer, Rational)])

addTripleToDistributionList (i1, n1, r1) (c, pairs@((n2, r2) : _)) = (r1, (n1, r1 - c) : pairs)

data Threshold =
  Threshold
    { theBoundA :: !Rational
    , theBoundB :: !Rational
    , theCountA :: !Integer
    , theCountB :: !Integer
    }
  deriving Show

data IntervalSpec =
  IntervalSpec
    { theBound :: !Rational
    , theCount :: !Integer
    }

getThresholds :: Integer -> [Threshold]
getThresholds weight
  | weight < 4 = [Threshold 0 1 0 weight]
  | True =
      let squareRoot = sqrt (fromInteger weight)
          hyperRoot = sqrt squareRoot
          intervals = max 1 (floor squareRoot)
          spacing = weight % intervals
          boundaries = [0, spacing .. (fromInteger weight) - ((4%5) * spacing)]
          intervalSpecs = map (getIntervalSpec weight spacing squareRoot hyperRoot) boundaries
          summedIntervalSpecs = foldl' sumIntervalSpecs [] intervalSpecs
      in foldl' combineIntervalSpecs [] summedIntervalSpecs

getIntervalSpec :: Integer -> Rational -> Double -> Double -> Rational -> IntervalSpec
getIntervalSpec weight spacing squareRoot hyperRoot 0 =
  IntervalSpec (spacing / (fromInteger weight)) (max 1 (floor spacing))
getIntervalSpec weight spacing squareRoot hyperRoot boundary =
  let squareRootOfBoundary = (fromRational boundary) / squareRoot
      subIntervals = max 1 (floor (hyperRoot / squareRootOfBoundary))
  in IntervalSpec ((boundary + spacing) / (fromInteger weight)) subIntervals

sumIntervalSpecs :: [IntervalSpec] -> IntervalSpec -> [IntervalSpec]
sumIntervalSpecs [] (IntervalSpec b i) = [IntervalSpec b i, IntervalSpec 0.0 0]
sumIntervalSpecs lastList@((IntervalSpec lastBoundary lastCount) : _) (IntervalSpec nextBoundary nextCount) =
  (IntervalSpec nextBoundary (nextCount + lastCount)) : lastList

combineIntervalSpecs :: [Threshold] -> IntervalSpec -> [Threshold]
combineIntervalSpecs [] (IntervalSpec b i) = [Threshold b b i i]
combineIntervalSpecs lastList@((Threshold lastBound _ lastCount _) : _) (IntervalSpec b i) =
  (Threshold b lastBound i lastCount) : lastList

continuedFractionExpandR :: Rational -> Rational -> Bool -> Bool -> [Integer]
continuedFractionExpandR a b includeA includeB
  | a > b = continuedFractionExpandR b a includeB includeA
  | a < b =
      let ceilA = ceiling a
          ceilARational = fromInteger ceilA
          floorB = (floor b)
          floorBRational = fromInteger floorB
          candidate = if includeA || (ceilARational > a) then ceilA else ceilA + 1
          candidateRational = fromInteger candidate
      in if (candidate <= floorB) && (includeB || (candidateRational < b))
           then [candidate]
           else
             let floorA = (floor a)
                 floorARational = fromInteger floorA
                 invertedUpperBound = (1 / (b - floorARational))
             in if a <= floorARational
                  then [floorA, ((floor invertedUpperBound) + 1)]
                  else floorA : (continuedFractionExpandR invertedUpperBound (1 / (a - floorARational)) includeB includeA)
  | True = error "A trivial interval cannot be expanded to a continued fraction"

continuedFractionExpand :: Double -> Double -> Bool -> Bool -> [Integer]
continuedFractionExpand a b includeA includeB =
  continuedFractionExpandR (toRational a) (toRational b) includeA includeB

continuedFractionCompute :: [Integer] -> Rational
continuedFractionCompute [i] = i % 1
continuedFractionCompute (i : items) =
  let frac = (recip $ continuedFractionCompute items)
  in frac `seq` (i % 1) + frac

testContinuedFraction :: Double -> Double -> Bool -> Bool -> IO ()
testContinuedFraction a b includeA includeB =
  do let continuedFraction = continuedFractionExpand a b includeA includeB
         rational = continuedFractionCompute continuedFraction
     putStrLn $ ""
     logVerbose "Interval" $ [shows includeA, shows a, shows b, shows includeB]
     logVerbose "Continued fraction expand" [shows $ continuedFraction]
     logVerbose "Corresponding rational" [shows rational, shows (fromRational rational)]

updateRationals :: Integer -> [(Integer, Rational)] -> Integer -> [(Integer, Rational)]
updateRationals previousWeight previousRationals w
  | w <= previousWeight = previousRationals
  | True =
      let smallerRationals = updateRationals previousWeight previousRationals (w - 1)
          boundaries = sort $ map snd smallerRationals
          intervals = zip (0 % 1 : boundaries) boundaries
          thresholds = getThresholds w
          Threshold _ _ _ weight = last thresholds
      in seq
           (fst $ head smallerRationals)
           ( if 3 * weight <= 2 * (toInteger $ length smallerRationals)
             then smallerRationals
             else let mapped = mapToThresholds thresholds boundaries
                      (((msa, sa), (msb, sb)) : mappedIntervals) = zip ((0, 0) : mapped) mapped
                      MappedInterval _ a _ b = foldl' maxMappedInterval (MappedInterval msa sa msb sb) mappedIntervals
                      (aa, bb) = scaleInterval (1%3) (a, b)
                      extra = continuedFractionCompute $ continuedFractionExpandR aa bb False False
                  in extra `seq` ((w, extra) : smallerRationals)
           )

getRationals :: Integer -> [(Integer, Rational)]
getRationals w = updateRationals 1 [(1, 1%1)] w

mapToThresholds :: [Threshold] -> [Rational] -> [(Rational, Rational)]
mapToThresholds _ [] = []
mapToThresholds thresholds@((Threshold boundA boundB intA intB) : moreThresholds) rationals@(x : moreRationals)
  | x > boundB = mapToThresholds moreThresholds rationals
  | x > boundA =
     let width = boundB - boundA
         count = fromInteger (intB - intA)
         mapped = (((x - boundA) * count) / width) + (fromInteger intA)
     in mapped `seq` (mapped, x) : mapToThresholds thresholds moreRationals
  | True = error $ "Rational is too small: " ++ (show x) ++ " < " ++ (show boundA)
mapToThresholds [] (x : _) = error $ "Rational is too big: " ++ (show x)

data MappedInterval = MappedInterval !Rational !Rational !Rational !Rational

maxMappedInterval :: MappedInterval -> ((Rational, Rational), (Rational, Rational)) -> MappedInterval
maxMappedInterval current@(MappedInterval ma a mb b) ((mc, c), (md, d)) =
  if md - mc > mb - ma
    then MappedInterval mc c md d
    else current

maxMappedIntervalOld :: ((Rational, Rational), (Rational, Rational)) -> ((Rational, Rational), (Rational, Rational)) -> ((Rational, Rational), (Rational, Rational))
maxMappedIntervalOld ((ma, a), (mb, b)) ((mc, c), (md, d)) =
  if md - mc > mb - ma
    then ((mc, c), (md, d))
    else ((ma, a), (mb, b))

scaleInterval :: Rational -> (Rational, Rational) -> (Rational, Rational)
scaleInterval r (a, b) =
  let w = b - a
      ww = r * w
      aa = a + ((w - ww) * (1%2))
      bb = aa + ww
  in (aa, bb)

-- |Container for probability thresholds ordered by the weights of the histograms
-- they apply to, and segmented in order to reduce the time needed to optimize
-- small histograms.
data RationalSegments = RationalSegments { theOffset :: !Integer, theSegmentList :: !RationalSegmentList } deriving (Eq, Read, Show)
type RationalSegmentList = [(Integer,[(Integer,Rational)])]

replaceMax :: RationalSegmentList -> Integer -> RationalSegmentList

replaceMax oldSegmentList@((oldMax, oldSegment) : otherSegmentList) newMax
  | newMax <= oldMax = oldSegmentList
  | newMax > oldMax = (newMax, oldSegment) : otherSegmentList

addToSegmentList :: RationalSegmentList -> Integer -> [(Integer, Rational)] -> (Integer, RationalSegmentList)

addToSegmentList oldRationalSegmentList@((oldMax, _) : _) segmentBound (pair@(idx, _) : otherRationals)
  | idx <= oldMax = (segmentBound, oldRationalSegmentList)
  | idx > oldMax =
      let (newSegmentBound, previousRationalSegmentList) = addToSegmentList oldRationalSegmentList segmentBound otherRationals
      in if idx > newSegmentBound
           then (newSegmentBound * 2, ((idx, [pair]) : (replaceMax previousRationalSegmentList newSegmentBound)))
           else let ((oldMax, oldSegment) : otherSegmentList) = previousRationalSegmentList
                in (newSegmentBound, (idx, (pair : oldSegment)) : otherSegmentList)

addToSegmentList oldRationalSegmentList segmentBound [] = (segmentBound, oldRationalSegmentList)

addToSegmentList [] _ _ = error "Missing initial segment"

-- |Adds probability thresholds to the given list of segments to accomodate the
-- optimization of larger histograms. Returns the original list of segments if no
-- new thresholds need to be added.
initialSegmentBound = 64

updateRationalSegments :: Integer -> RationalSegments -> Maybe RationalSegments

updateRationalSegments w oldRationalSegments =
  do let oldRationalSegmentList = theSegmentList oldRationalSegments
     newRationalSegments <- updateRationalSegmentList oldRationalSegmentList w
     return $ oldRationalSegments { theSegmentList = newRationalSegments }

updateRationalSegmentList :: RationalSegmentList -> Integer -> Maybe RationalSegmentList

updateRationalSegmentList previousRationalSegmentList@((previousWeight, _) : ((penultimateWeight, _) : _)) w
  | w <= previousWeight = Nothing
  | True =
      let previousRationals = getAllRationalPairsFromSegmentList previousRationalSegmentList
          newRationals = updateRationals previousWeight previousRationals w
          segmentBound =
            if penultimateWeight < 1
              then initialSegmentBound
              else penultimateWeight * 2
          (_, newSegmentList) = addToSegmentList previousRationalSegmentList segmentBound newRationals
       in Just $ replaceMax newSegmentList w

updateRationalSegmentList (segment : []) w = updateRationalSegmentList [segment, (0,[])] w

updateRationalSegmentList [] w = 
  let basicSegment = [(initialSegmentBound, getRationals initialSegmentBound)]
      more = updateRationalSegmentList basicSegment w
  in case more of
       Nothing -> Just basicSegment
       Just _ -> more

getRationalPairsFromList :: [(Integer,Rational)] -> Integer -> [(Integer, Rational)]

getRationalPairsFromList allRationals@((idx, _) : otherRationals) maxIndex
  | maxIndex >= idx = allRationals
  | maxIndex < idx = getRationalPairsFromList otherRationals maxIndex

getRationalPairsFromList [] _ = []

getRationalPairsFromSegments :: RationalSegments -> Integer -> [(Integer, Rational)]

getRationalPairsFromSegments oldRationalSegments w =
  let oldRationalSegmentList = theSegmentList oldRationalSegments
  in getRationalPairsFromSegmentList oldRationalSegmentList w

getRationalPairsFromSegmentList :: RationalSegmentList -> Integer -> [(Integer, Rational)]

getRationalPairsFromSegmentList _ 0 = []
getRationalPairsFromSegmentList ((_, segment) : otherSegmentList@((penultimateBound, penultimateSegment) : _)) w
  | w >= penultimateBound =
      let initialPart =
            if w > penultimateBound
              then getRationalPairsFromList segment w
              else []
      in initialPart ++ (getAllRationalPairsFromSegmentList otherSegmentList)
  | w < penultimateBound = getRationalPairsFromSegmentList otherSegmentList w
getRationalPairsFromSegmentList [segment] w = getRationalPairsFromSegmentList [segment, (0, [])] w
getRationalPairsFromSegmentList s w = error $ "No rational pairs segments for: " ++ (show w) ++ ": " ++ (show s)

getAllRationalPairsFromSegmentList :: RationalSegmentList -> [(Integer, Rational)]

getAllRationalPairsFromSegmentList segments =
  concatMap snd segments

getRationalsFromSegments :: RationalSegments -> Integer -> [Rational]

getRationalsFromSegments oldRationalSegments w = getRationalsFromSegmentList (theSegmentList oldRationalSegments) w

getRationalsFromSegmentList :: RationalSegmentList -> Integer -> [Rational]

getRationalsFromSegmentList segments w = sort $ map snd $ getRationalPairsFromSegmentList segments w

intervalIncludes :: Rational -> Rational -> (Rational, Rational) -> Bool
intervalIncludes a b (_, x) = x > a && x <= b

oracleHeader = "--LExAu Oracle Data"

readPairs :: Handle -> Maybe Integer -> (Integer, RationalSegmentList) -> IO (Integer, RationalSegmentList)

readPairs oracleHandle maybeLimit oldState@(oldSegmentBound, oldSegmentList) =
  do endOfOracle <- hIsEOF oracleHandle
     if endOfOracle
       then return oldState
       else
         do let updateState curState@(curSegmentBound, curSegmentList) line =
                  let pair@(weight, _) = fst $ head $ reads line
                  in (weight, addToSegmentList curSegmentList curSegmentBound [pair])
            (weight, newState@(_, newSegmentList)) <- (updateState oldState) `fmap` (hGetLine oracleHandle)
            continue <- return $
              case maybeLimit of
                Nothing -> True
                Just limit -> weight < limit
            (fst $ head $ snd $ head $ newSegmentList) `seq`
              if continue
                then readPairs oracleHandle maybeLimit newState
                else return $! newState

computeAndSave :: Handle -> Integer -> RationalSegmentList -> IO ()

computeAndSave oracleHandle oldWeight oldSegmentList =
  do let newWeight = oldWeight + 1
         maybeNewSegmentList = updateRationalSegmentList oldSegmentList newWeight
         newSegmentList = fromMaybe oldSegmentList maybeNewSegmentList
     case maybeNewSegmentList of
       Just ((_, (pair@(w, _) : _)) : _) ->
         do if w == newWeight
              then
                do hPutStrLn oracleHandle $ show pair
                   hFlush oracleHandle
              else return ()
       Nothing -> return ()
     computeAndSave oracleHandle newWeight newSegmentList

-- |Precomputes interval boundaries for MDL the optimizer.
precompute :: String -> IO ()
precompute oracleFile =
  bracket
    (openFile oracleFile ReadWriteMode)
    hClose
    (\ oracleHandle ->
      do emptyOracle <- hIsEOF oracleHandle
         (initialWeight, initialSegmentList) <-
           if emptyOracle
             then
               do let freshInitialSegmentList = fromMaybe [] $ updateRationalSegmentList [] 2
                      freshPairs = reverse $ getAllRationalPairsFromSegmentList freshInitialSegmentList
                      (freshWeight, _) = head freshPairs
                      freshShowPairs = map (\x -> shows x . showString "\n") freshPairs
                      freshShows = foldl' (.) id freshShowPairs
                  putStrLn "Fresh oracle"
                  hPutStrLn oracleHandle oracleHeader
                  hPutStr oracleHandle (freshShows "")
                  return (freshWeight, freshInitialSegmentList)
             else
               do let maxWeight segments = fst $ head $ snd $ head segments
                  header <- hGetLine oracleHandle
                  if header /= oracleHeader
                    then error $ "Not a LExAu oracle data file: '" ++ oracleFile ++ "'"
                    else return ()
                  startPair@(_, startSegmentList) <- readPairs oracleHandle Nothing (initialSegmentBound, fromMaybe [] $ updateRationalSegmentList [] 2)
                  logVerbose "Segments read from oracle file" [shows startSegmentList]
                  return (maxWeight startSegmentList, startSegmentList)
         logVerbose "Initial weight" [shows initialWeight]
         hFlush stdout
         computeAndSave oracleHandle initialWeight initialSegmentList
         return ()
    )

updateRationalSegmentsIO ::
    String
      -> Maybe Integer
      -> RationalSegments
      -> IO (Maybe RationalSegments)

updateRationalSegmentsIO location maybeLimit oldState =
  let oldSegmentList = theSegmentList oldState
      maxWeight segments = fst $ head $ snd $ head segments
      oldMaxWeight = maxWeight oldSegmentList
      trivial =
        case maybeLimit of
          Just limit -> limit <= oldMaxWeight
          Nothing -> False
  in if trivial
       then return Nothing
       else
         bracket
           (openFile location ReadMode)
           (hClose)
           (\oracleHandle ->
             do let oldOffset = theOffset oldState
                header <- hGetLine oracleHandle
                if header /= oracleHeader
                  then error $ "Not a LExAu oracle data file: '" ++ location ++ "'"
                  else return ()
                if oldOffset > 0
                  then hSeek oracleHandle AbsoluteSeek oldOffset
                  else return ()
                (_, newSegmentList) <- readPairs oracleHandle maybeLimit (fst $ head $ oldSegmentList, oldSegmentList)
                newMaxWeight <- return $ maxWeight newSegmentList
                if newMaxWeight <= oldMaxWeight
                  then return $ Nothing
                  else
                    do newOffset <- hTell oracleHandle
                       return $! Just $! oldState { theOffset = newOffset, theSegmentList = newMaxWeight `seq` newSegmentList }
           )

updateLimitedRationalSegmentsIO ::
    String
      -> Integer
      -> RationalSegments
      -> IO (Maybe RationalSegments)

updateLimitedRationalSegmentsIO location limit oldState =
  updateRationalSegmentsIO location (Just limit) oldState

updateUnlimitedRationalSegmentsIO ::
    String
      -> RationalSegments
      -> IO (Maybe RationalSegments)

updateUnlimitedRationalSegmentsIO location oldState =
  updateRationalSegmentsIO location Nothing oldState

createMDLOptimizerFromCache ::
      ( BaseHistogram observedDistributionType
      , IntDistribution observedDistributionType
      ) =>
    String
      -> DistributionFactory observedDistributionType
      -> OptimizerType observedDistributionType MDLDistributionImpl RationalSegments

createMDLOptimizerFromCache location distributionFactory =
  let Just initialOracle = updateRationalSegmentList [] 1
      updater = updateLimitedRationalSegmentsIO location
      initialSegments = RationalSegments 0 initialOracle
  in createOptimizerIO initialSegments updater distributionFactory optimize

infoRationals :: Integer -> [(Rational, Rational)] -> [(Rational, Rational, Integer, Integer)] -> Threshold -> [(Rational, Rational, Integer, Integer)]
infoRationals w intervals acc (Threshold boundA boundB intA intB) =
  let insideInterval = filter (intervalIncludes boundA boundB) $ intervals
      nrDefined = toInteger $ length insideInterval
  in (boundA, boundB, intB - intA, nrDefined) : acc

testWeight :: Integer -> IO ()
testWeight weight =
  do putStrLn $ ""
     logVerbose "Thresholds" [shows weight, showListVertically $ getThresholds weight]

testGetRationals :: Integer -> IO ()
testGetRationals w =
  do let rationals = getRationals w
     putStrLn $ ""
     logVerbose "Get rationals" [shows w, showListVertically $ rationals]

testMDL :: String -> Int -> DistributionType MDLDistributionImpl -> IO ()
testMDL id maxKeyIdx expectedDistribution =
  do let pair = (Just $ [1..maxKeyIdx], expectedDistribution)
         code = prefixEncode pair
         codeTerse = toTerseCode $ code ""
         pairDescriptionLength = descriptionLength pair
     logVerbose "Prefix encoding" [showString id, code, showString codeTerse, shows $ length codeTerse, shows pairDescriptionLength]

test :: Maybe String -> IO ()
test maybeOptimizerCacheLocation =
  do putStrLn ""
     putStrLn $ "Test LExAu.Distribution.MDL"

     putStrLn $ ""
     testContinuedFraction 1.25 1.303 False False
     testContinuedFraction 1.25 1.303 True False
     testContinuedFraction 1.24 1.25 False False
     testContinuedFraction 1.241 1.25 False True
     testContinuedFraction 1.24e-20 1.25e-20 True True
     testContinuedFraction 1.0000000001e-5 1.0000000009e-5 True False
     putStrLn $ ""
     logVerbose "Approximate rational: 0.24" [shows $ toRational 0.24e-20, shows 0.24e-20, shows $ fromRational $ toRational 0.24e-20]

     foldr (>>) (return ()) $ map testWeight [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 1000]
     foldr (>>) (return ()) $ map testGetRationals [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 1000]

     putStrLn $ ""
     do let w = 1000
            smallerRationals = getRationals (w - 1)
            boundaryMarkers = sortBy (\p q -> (snd p) `compare` (snd q)) smallerRationals
            boundaries = map snd boundaryMarkers
            intervals = zip (0 % 1 : boundaries) boundaries
            nrIntervals = length intervals
            thresholds = getThresholds w
            info = foldl' (infoRationals w intervals) [] thresholds
            nrInfoIntervals = sum $ map (\(_,_,_,i) -> i) info
            mapped = mapToThresholds thresholds boundaries
            (((msa, sa), (msb, sb)) : mappedIntervals) = zip ((0, 0) : mapped) mapped
            MappedInterval _ a _ b = foldl' maxMappedInterval (MappedInterval msa sa msb sb) mappedIntervals
            (aa, bb) = scaleInterval (1%3) (a, b)
            extra = continuedFractionCompute $ continuedFractionExpandR aa bb False False
            result = (w, extra) : smallerRationals
            Just initialSegments = updateRationalSegmentList [] 1900
            segmentList = fromMaybe initialSegments $ updateRationalSegmentList initialSegments 2000
            segments = RationalSegments 0 segmentList
            smallRationals = getRationalsFromSegmentList segmentList 10
            bigRationals = getRationalsFromSegmentList segmentList 300
        logVerbose "Continued fraction" [shows (1%3), shows (2%3), shows $ continuedFractionExpandR (1%3) (2%3) False False]
        logVerbose "Smaller rationals" [showListVertically $ smallerRationals]
        logVerbose "Boundary markers" [showListVertically $ boundaryMarkers]
        logVerbose "Intervals" [showListVertically $ intervals]
        logVerbose "Thresholds" [showListVertically $ thresholds]
        logVerbose "Result" [showListVertically $ result]
        logVerbose "Info" [shows nrIntervals, shows nrInfoIntervals, showListVertically $ info]
        logVerbose "Mapped boundaries" [showListVertically $ mapped]
        logVerbose "Initial segments" [showListVertically initialSegments]
        logVerbose "Segment list" [showListVertically segmentList]
        logVerbose "Small rationals" [showListVertically smallRationals]
        logVerbose "Big rationals" [showListVertically $ map (\x -> 1000.0 + fromRational x) bigRationals]

     case maybeOptimizerCacheLocation of
       Nothing -> return ()
       Just optimizerCacheLocation ->
         do let optimizerFactory = createMDLOptimizerFromCache optimizerCacheLocation
                startOptimizerState = optimizerFactory (DistributionFactory $ Just histogramFromList)
            logVerbose "Start optimizer state" [shows startOptimizerState]
            nextOptimizerState <- fromJust `liftM` updateOptimizerIO 1000 startOptimizerState
            logVerbose "Next optimizer state" [shows nextOptimizerState]
            lastOptimizerState <- fromJust `liftM` updateOptimizerIO 1100 nextOptimizerState
            logVerbose "Last optimizer state" [shows lastOptimizerState]
            return ()

     do let trivialObservedDistribution = histogramFromList [(2,37)]
            trivialSparseObservedDistribution = histogramFromList [(32,37)]
            observedDistribution = histogramFromList [(1, 0), (2, 11), (3, 4), (4, 7)]
            distWeight = weight observedDistribution
            Just segmentList = updateRationalSegmentList [] distWeight
            segments = RationalSegments 0 segmentList
            unsortedRationals = getRationalPairsFromSegmentList segmentList distWeight
            rationals = sortBy (\(_, r1) (_, r2) -> r1 `compare` r2) unsortedRationals
            pairs = distributionToPairs observedDistribution
            selected = selectRationals pairs rationals
            deltaList = triplesToDistributionList $ reverse selected
            sortedDeltaList = tail $ sortBy (\(n1, r1) (n2, r2) -> r1 `compare` r2) deltaList
            zipped = zip pairs sortedDeltaList
            expectedDistributionList = map (\((i, _), (n, r)) -> (i, (n, r))) zipped
            expectedDistribution = optimize observedDistribution segments :: DistributionType MDLDistributionImpl
            trivialExpectedDistribution = optimize trivialObservedDistribution segments :: DistributionType MDLDistributionImpl
            trivialSparseExpectedDistribution = optimize trivialSparseObservedDistribution segments :: DistributionType MDLDistributionImpl
            alphabet = createAlphabet "plankje" ["aap", "noot", "mies", "boot"]
            noot = alphabet `member` "noot"

        putStrLn $ ""
        logVerbose "Observed distribution" [logs observedDistribution]
        logVerbose "Segments" [shows segments]
        logVerbose "Rationals" [shows rationals]
        logVerbose "Pairs" [shows pairs]
        logVerbose "Selected" [showListVertically selected]
        logVerbose "Sorted delta list" [showListVertically sortedDeltaList]
        logVerbose "Zipped" [showListVertically zipped]
        logVerbose "Expected distribution list" [showListVertically expectedDistributionList]
        logVerbose "Shows expected distribution" [shows expectedDistribution]
        logVerbose "Log distribution expected distribution" [logDistribution alphabet expectedDistribution]
        logVerbose "Probability" [logs noot, shows $ probability expectedDistribution noot]
        testMDL "tight" 4 expectedDistribution
        testMDL "dense" 3 trivialExpectedDistribution
        testMDL "dense" 7 expectedDistribution
        testMDL "sparse" 32 trivialExpectedDistribution
        testMDL "tight" 32 trivialSparseExpectedDistribution
        testMDL "sparse" 32 expectedDistribution
        stdGen <- getStdGen
        (randomWords, nextGen) <- return $ randomSequence alphabet expectedDistribution stdGen 30
        print $ map name randomWords

-- EOF
