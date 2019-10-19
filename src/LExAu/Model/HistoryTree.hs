{-# LANGUAGE FlexibleContexts #-}
module LExAu.Model.HistoryTree
  ( ReadExpectedContext(..)
  , ReadModelContext(..)
  , ReadProcessContext(..)
  , createEmptyHistoryTreeExpected
  , createEmptyHistoryTreeModel
  , createEmptyHistoryTreeProcess
  , createHistoryTreeExpected
  , createHistoryTreeModel
  , createHistoryTreeProcess
  , getModelOfObservedBehavior
  , getUpdateSteps
  , test
  ) where

import Control.Monad (liftM)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef)
import System.Random (getStdGen)

import LExAu.API.Alphabet
  ( Alphabet(addSymbol)
  , DirectedSymbol(DirectedSymbol, theDirection, theSymbol),
  )
import LExAu.API.DescriptionLength
  ( DescriptionLength
  , PrefixEncode
  )
import LExAu.API.Distribution
  ( DistributionFactory(DistributionFactory)
  , DistributionType
  , Histogram
  , IndexedHistogram
  , IntDistribution
  , LogDistribution
  )
import LExAu.API.HistoryTree
  ( HistoryTreeModel
      ( clearUpdated
      , increaseHistoryTree
      , increaseHistoryTreeST
      , incrementHistoryTree
      , incrementHistoryTreeST
      , markExtensible
      , markExtensibleST
      , maximumDepth
      )
  , UpdateMarkers(UpdateMarkers)
  )
import LExAu.API.Indexed (IndexedMember, member)
import LExAu.API.Interaction (offerActionST, offerDirectedSymbolST)
import LExAu.API.MDL
  ( OptimizerType
  )
import LExAu.API.Model
  ( AlphabetUpdate(..)
  , OptimizeUpdate(..)
  , Updater(update)
  , UpdaterIO(updateIO)
  )
import LExAu.API.Named (Named(name))
import LExAu.API.ReaderContext (ReaderContext(readsWithContext))
import LExAu.Alphabet.Directed (createDirectedAlphabet)
import LExAu.Alphabet.Standard (createAlphabet)
import LExAu.Distribution.Histogram (histogramFromList)
import LExAu.Model.HistoryTreeImpl.Covered
  ( CoveredUpdate
  )
import LExAu.Model.HistoryTreeImpl.Expected
  ( UpdateStep
  , breakUp
  , createEmptyHistoryTreeExpected
  , createHistoryTreeExpected
  )
import LExAu.Model.HistoryTreeImpl.ExpectedData
  ( HistoryTreeExpectedImpl
  , ReadExpectedContext(..)
  )
import LExAu.Model.HistoryTreeImpl.HTData (HTStatus(..))
import LExAu.Model.HistoryTreeImpl.Observed
  ( HistoryTreeModelImpl(..)
  , ReadModelContext(..)
  , createHistoryTreeModel
  , createEmptyHistoryTreeModel
  )
import LExAu.Model.HistoryTreeImpl.Process
  ( HistoryTreeProcessImpl(..)
  , ReadProcessContext(..)
  , createEmptyHistoryTreeProcess
  , createHistoryTreeProcess
  , getModelOfObservedBehavior
  , markProcessExtensibleST
  )
import LExAu.Model.HistoryTreeImpl.Solid
  ( setSolid
  )
import LExAu.Utilities.Logging
  ( Loggable(logs)
  , Summarize
  , logVerbose
  )
import qualified LExAu.Alphabet.Directed as Directed (test)

getUpdateSteps
  :: (
    Alphabet alphabetType symbolType
  )
  => Int
  -> (HistoryTreeProcessImpl alphabetType symbolType observedDistributionType)
  -> [UpdateStep alphabetType symbolType observedDistributionType]

getUpdateSteps chunkSize process =
  breakUp chunkSize $ getModelOfObservedBehavior process

doubleDir :: (DirectedSymbol symbolType) -> (DirectedSymbol (DirectedSymbol symbolType))
doubleDir symbol = (DirectedSymbol (theDirection symbol) symbol)

maybeTestSetSolid :: (
      Alphabet alphabetType symbolType,
      IndexedMember symbolType Int,
      IndexedHistogram observedDistributionType,
      Histogram alphabetType symbolType observedDistributionType,
      Updater CoveredUpdate (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) UpdateMarkers,
      Eq expectedDistributionType,
      IntDistribution expectedDistributionType,
      DescriptionLength (Maybe [Int], DistributionType expectedDistributionType),
      PrefixEncode (Maybe [Int], DistributionType expectedDistributionType),
      LogDistribution alphabetType symbolType observedDistributionType,
      Show (DistributionType observedDistributionType),
      LogDistribution alphabetType symbolType expectedDistributionType,
      Show (DistributionType expectedDistributionType),
      Summarize (OptimizerType observedDistributionType expectedDistributionType oracleType)
    ) =>
  [symbolType] -> HTStatus -> Maybe (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType) -> IO (Maybe (HistoryTreeExpectedImpl alphabetType symbolType observedDistributionType expectedDistributionType oracleType))

maybeTestSetSolid path status maybeOldExpected =
  do let pathNames = map name path
     logVerbose "Path" [logs path]
     maybeResult <- return $ maybeOldExpected >>= (setSolid path $ const status)
     maybeSolidified <- return $ liftM fst maybeResult
     logVerbose "Maybe solidified" [shows pathNames, shows $ liftM snd maybeResult, logs maybeSolidified]
     return $! maybeSolidified

testIncrementAndExtend :: (HistoryTreeModel historyTreeType symbolType) => [symbolType] -> symbolType -> historyTreeType -> historyTreeType
testIncrementAndExtend history nextResponse oldModel =
  let incrementedModel = incrementHistoryTree oldModel history nextResponse
  in markExtensible incrementedModel history

testRepeatedIncrementAndExtend :: (HistoryTreeModel historyTreeType symbolType) => Int -> [symbolType] -> symbolType -> historyTreeType -> historyTreeType
testRepeatedIncrementAndExtend count history nextResponse oldModel =
  last $ take (count + 1) $ iterate (testIncrementAndExtend history nextResponse) oldModel

test =
  do let alphabet = createAlphabet "small" ["", "a", "b"]
         empty = alphabet `member` ""
         a = alphabet `member` "a"
         b = alphabet `member` "b"

         factory = DistributionFactory $ Just histogramFromList

         emptyModel = createEmptyHistoryTreeModel factory alphabet
         tinyModel = increaseHistoryTree emptyModel [empty, empty] empty 4
         modelA = testRepeatedIncrementAndExtend 3 [empty, empty] a tinyModel
         modelB = testRepeatedIncrementAndExtend 3 [a, b, a, b] a modelA
         modelBB = increaseHistoryTree (clearUpdated modelB) [a, b, a, b] a 2
         modelC = increaseHistoryTree (clearUpdated modelBB) [a, b] b 3
         readModelContext = ReadModelContext createAlphabet createEmptyHistoryTreeModel factory
         copyOfModelBB = fst $ head $ readsWithContext readModelContext $ show modelBB

         actions = createAlphabet "action" ["left", "right"]
         responses = createAlphabet "response" ["dark", "light"]
         symbols = createDirectedAlphabet "symbol" actions responses
         left = doubleDir $ symbols `member` ">left"
         right = doubleDir $ symbols `member` ">right"
         dark = doubleDir $ symbols `member` "<dark"
         light = doubleDir $ symbols `member` "<light"
         interactiveModelStart = createEmptyHistoryTreeModel factory symbols
         (newSymbols, dim) = addSymbol symbols "<dim"

         emptyExpected = createEmptyHistoryTreeExpected newSymbols factory Nothing
     putStrLn $ ""
     putStrLn $ "Test LExAu.Model.HistoryTree"
     logVerbose "Empty model" [logs emptyModel]
     logVerbose "Tiny model" [logs tinyModel]
     logVerbose "Model A" [logs modelA]
     logVerbose "Model B" [logs modelBB]
     logVerbose "Model C" [logs modelC]
     logVerbose "Serialized model B" [shows modelBB]
     logVerbose "Copy of model B" [logs copyOfModelBB]
     stRefModel <- return $ runST (
         do modelRef <- newSTRef emptyModel
            increaseHistoryTreeST modelRef [empty, empty] empty 4
            incrementHistoryTreeST modelRef [empty, empty] a
            markExtensibleST modelRef [empty, empty]
            incrementHistoryTreeST modelRef [empty, empty] a
            markExtensibleST modelRef [empty, empty]
            incrementHistoryTreeST modelRef [empty, empty] a
            markExtensibleST modelRef [empty, empty]
            incrementHistoryTreeST modelRef [a, b, a, b] a
            markExtensibleST modelRef [a, b, a, b]
            incrementHistoryTreeST modelRef [a, b, a, b] a
            markExtensibleST modelRef [a, b, a, b]
            incrementHistoryTreeST modelRef [a, b, a, b] a
            markExtensibleST modelRef [a, b, a, b]
            increaseHistoryTreeST modelRef [a, b, a, b] a 2
            increaseHistoryTreeST modelRef [a, b] b 3
            readSTRef modelRef
       )
     logVerbose "STRef model" [logs $ clearUpdated stRefModel]
     logVerbose "Maximum depth of STRef model" [shows $ maximumDepth stRefModel]

     interactiveModelUpdatedAlphabet <- return $ update AlphabetUpdate newSymbols interactiveModelStart
     logVerbose "Interactive model with updated alphabet" [shows interactiveModelUpdatedAlphabet]

     stdGen <- getStdGen
     processStart <- return $ createHistoryTreeProcess modelC stdGen
     logVerbose "Process start" [logs processStart]
     logVerbose "Dark" [shows dark]
     processNext <- return $ runST (
         do processRef <- newSTRef processStart
            offerActionST processRef a
            offerActionST processRef b
            offerActionST processRef a
            offerActionST processRef b
            offerActionST processRef a
            offerActionST processRef empty
            readSTRef processRef
       )
     logVerbose "Process next" [logs processNext]
     Directed.test

     interactiveProcessStart <- return $ createHistoryTreeProcess interactiveModelStart stdGen
     interactiveProcessNext <- return $ runST (
         do processRef <- newSTRef interactiveProcessStart
            offerDirectedSymbolST processRef dark
            offerDirectedSymbolST processRef left
            offerDirectedSymbolST processRef dark
            offerDirectedSymbolST processRef left
            offerDirectedSymbolST processRef dark
            offerDirectedSymbolST processRef right
            offerDirectedSymbolST processRef dark
            offerDirectedSymbolST processRef right
            markProcessExtensibleST processRef [theSymbol right]
            offerDirectedSymbolST processRef light
            offerDirectedSymbolST processRef right
            offerDirectedSymbolST processRef dark
            offerDirectedSymbolST processRef right
            offerDirectedSymbolST processRef light
            offerDirectedSymbolST processRef left
            offerDirectedSymbolST processRef dark
            offerDirectedSymbolST processRef left
            offerDirectedSymbolST processRef dark
            offerDirectedSymbolST processRef right
            markProcessExtensibleST processRef [theSymbol right, theSymbol dark]
            offerDirectedSymbolST processRef dark
            offerDirectedSymbolST processRef right
            offerDirectedSymbolST processRef light
            offerDirectedSymbolST processRef right
            offerDirectedSymbolST processRef dark
            offerDirectedSymbolST processRef right
            offerDirectedSymbolST processRef light
            readSTRef processRef
       )
     logVerbose "Interactive process next" [shows interactiveProcessNext]
     logVerbose "Interactive process next" [logs interactiveProcessNext]

     logVerbose "Empty expected" [shows emptyExpected]
     updatedModel <- return $ theModel interactiveProcessNext

     logVerbose "Symbols " [shows symbols]
     logVerbose "Symbol[5]" [shows $ (symbols `member` (5 :: Int))]
     alphabetOfUpdatedModel <- return $ theAlphabet updatedModel
     logVerbose "Alphabet stuff" [shows alphabetOfUpdatedModel, shows $ (alphabetOfUpdatedModel `member` (5 :: Int))]

     maybeUpdatedExpected <- updateIO OptimizeUpdate updatedModel emptyExpected
     logVerbose "Updated expected" [shows maybeUpdatedExpected]

     logVerbose "Empty expected" [logs emptyExpected]
     logVerbose "Updated expected" [logs maybeUpdatedExpected]

     maybeTestSetSolid [theSymbol left] Solid maybeUpdatedExpected
     path <- return $ [theSymbol right, theSymbol dark, theSymbol left]
     maybeSolid <- maybeTestSetSolid path Solid maybeUpdatedExpected
     maybeTestSetSolid path Virtual maybeSolid
     maybeDoubleSolid <- maybeTestSetSolid [theSymbol right] Solid maybeSolid
     maybeTestSetSolid [theSymbol right, theSymbol dark] Solid maybeDoubleSolid
     maybeTestSetSolid [theSymbol right, theSymbol light] Solid maybeDoubleSolid
     maybeTestSetSolid [theSymbol right, theSymbol dark] Solid maybeSolid

     return ()

-- EOF
