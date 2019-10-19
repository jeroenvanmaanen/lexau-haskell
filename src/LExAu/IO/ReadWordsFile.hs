{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- |Implementation of an Encounter that reads a file.
module LExAu.IO.ReadWordsFile
  ( createEncounter
  , showLastTest
  , testConcurrent
  , testDB
  ) where

import Control.Exception (bracket)
import Control.Monad (liftM)
import Control.Monad.ST (stToIO)
import Data.Char (ord)
import Data.List (isPrefixOf, isSuffixOf)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(ReadMode), hGetLine, hIsEOF, openFile)
import System.Posix.Files (getFileStatus, modificationTime)
import System.Posix.Directory (DirStream, closeDirStream, openDirStream, readDirStream)
import System.Posix.Types (EpochTime)
import System.Random (RandomGen(split), getStdGen)

import LExAu.API.Alphabet (Alphabet(addSymbol),allSymbols)
import LExAu.API.Distribution (DistributionFactory(DistributionFactory))
import LExAu.API.Encounter (
    Encounter(stepCount, stepIO, updateStrategyST)
  )
import LExAu.API.Indexed (Indexed(maybeMember,member))
import LExAu.API.Model
  ( EvaluateUpdate(..)
  , HasCheckSum(checkSum)
  , PolicyUpdate(..)
  , Updater(update)
  , UpdaterIO(updateIO)
  , pureUpdateToIO
  )
import LExAu.API.Named (Named(name))
import LExAu.API.ReaderContext (ReaderContext(readsWithContext))
import LExAu.Alphabet.Directed (DirectedAlphabet, createDirectedAlphabet)
import LExAu.Alphabet.Standard (createAlphabet)
import LExAu.Distribution.Histogram (histogramFromList)
import LExAu.Distribution.MDL (createMDLOptimizer)
import LExAu.IO.Words (alexScanTokens)
import LExAu.Model.HistoryTree
  ( ReadExpectedContext(..)
  , ReadModelContext(..)
  , createEmptyHistoryTreeExpected
  , createEmptyHistoryTreeModel
  , createEmptyHistoryTreeProcess
  , getUpdateSteps
  )
import LExAu.Model.HistoryTreeDBImpl.Habits(policyUpdateMethod)
import LExAu.Model.HistoryTreeDBImpl.Observed(createRecorderStateRef, maybeAddSymbol)
import LExAu.Model.HistoryTreeImpl.Expected (createHistoryTreeExpected)
import LExAu.Pipeline.Concurrent (PipelineContext(..), runPipeline)
import LExAu.Pipeline.MongoDB(runRecorder)
import LExAu.Utilities.Logging (LogFunction, Loggable(logs), logVerbose)
import LExAu.Utilities.MongoDB (createDbContext)

data NoPolicy = NoPolicy deriving (Show, Read)

instance HasCheckSum NoPolicy where
  checkSum _ = 0

instance Updater PolicyUpdate (EncounterImpl alphabetType symbolType) NoPolicy where
  update PolicyUpdate _ _ = Nothing

instance Updater EvaluateUpdate NoPolicy expectedUpdateType where
  update EvaluateUpdate _ _ = Nothing

instance UpdaterIO EvaluateUpdate NoPolicy expectedType where
  updateIO = pureUpdateToIO update

instance Loggable NoPolicy where
  logs _ = showString "NoPolicy"

data EncounterImpl alphabetType symbolType =
  EncounterImpl { environment :: Handle, alphabet :: alphabetType, stepCounter :: Integer, buffer :: [String] }

instance (
      Alphabet alphabetType symbolType
    ) =>
    Encounter (EncounterImpl alphabetType symbolType) NoPolicy alphabetType symbolType where

  stepCount encounter = stepCounter encounter

  stepIO encounterRef =
    do EncounterImpl environmentHandle oldAlphabet oldSteps oldBuffer <- stToIO $ readSTRef encounterRef
       (maybeToken, newBuffer) <-
         case oldBuffer of
           [] -> readNextLine environmentHandle
           (t : b) -> return $ (Just t, b)
       case maybeToken of
         Nothing -> return (Nothing, Nothing, Nothing)
         Just token ->
           do (maybeNewAlphabet, response) <- return $ offerWordName oldAlphabet $ tokenToName token
              newAlphabet <- return $
                case maybeNewAlphabet of
                  Nothing -> oldAlphabet
                  Just theNewAlphabet -> theNewAlphabet
              stToIO $ writeSTRef encounterRef (EncounterImpl environmentHandle newAlphabet (oldSteps + 1) newBuffer)
              return (Just response, Nothing, maybeNewAlphabet)

  updateStrategyST encounterRef policyPort = return True

readNextLine :: Handle -> IO (Maybe String, [String])
readNextLine environmentHandle =
  do isEOF <- hIsEOF environmentHandle
     if isEOF
       then return (Nothing, [])
       else
         do line <- hGetLine environmentHandle
            tokens <- return $ alexScanTokens line
            case tokens of
              (token : buffer) -> return $ (Just token, buffer)
              [] -> -- readNextLine environmentHandle
                    return (Just "\n\n", []) -- Paragraph

tokenToName :: String -> String
tokenToName = ('<' :) . concatMap charToName

charToName :: Char -> String
charToName char =
  case char of
    '\n' -> "&nl;"  -- new line
    '\r' -> "&cr;"  -- carriage return
    '\t' -> "&tab;" -- tab
    '&' -> "&amp;"  -- ampersand
    ' ' -> "_"      -- visible space
    '_' -> "&us;"   -- underscore
    _ ->
      if char < ' ' || char > '~'
        then (showString "&#" . shows (ord char)) ";" -- extended
        else [char] -- printable

offerWordName :: (Alphabet alphabetType symbolType) => alphabetType -> String -> (Maybe alphabetType, symbolType)
offerWordName oldAlphabet symbolName =
  case maybeMember oldAlphabet symbolName of
    Just symbolSymbol -> (Nothing, symbolSymbol)
    Nothing -> (Just newAlphabet, symbol) where (newAlphabet, symbol) = addSymbol oldAlphabet symbolName

createEncounter :: (
      Alphabet alphabetType symbolType
    ) =>
  String -> alphabetType -> IO (EncounterImpl alphabetType symbolType)

createEncounter fileName alphabet =
  do handle <- openFile fileName ReadMode
     return $ EncounterImpl handle alphabet 1 []

showLastTest :: LogFunction -> String -> IO ()
showLastTest logger exportDir =
  do logger "Export directory" [shows exportDir]
     maybeFilePath <- findMostRecentFile logger (\name -> "expected" `isPrefixOf` name && not ("~" `isSuffixOf` name)) exportDir
     logger "File path" [shows maybeFilePath]
     case maybeFilePath of
       Just filePath ->
         do let fakeCreateAlphabet name symbolNames = createDirectedAlphabet name symbols symbols
                  where symbols = createAlphabet name symbolNames
                observedDistributionFactory = DistributionFactory $ Just histogramFromList
                readModelContext = ReadModelContext fakeCreateAlphabet createEmptyHistoryTreeModel observedDistributionFactory
                optimizer = createMDLOptimizer observedDistributionFactory
                readExpectedContext = ReadExpectedContext readModelContext optimizer
            representation <- readFile filePath
            -- logger "Representation" [showString representation]
            logger "Last saved model of expected behavior" $ map (logs . fst) $ readsWithContext readExpectedContext representation
       _ -> return ()

findMostRecentFile :: LogFunction -> (FilePath -> Bool) -> FilePath -> IO (Maybe FilePath)
findMostRecentFile logger predicate directory =
  do bracket
       (openDirStream directory)
       (closeDirStream)
       (searchMostRecentFile logger directory predicate Nothing Nothing)

searchMostRecentFile :: LogFunction -> FilePath -> (FilePath -> Bool) -> Maybe EpochTime -> Maybe FilePath -> DirStream -> IO (Maybe FilePath)
searchMostRecentFile logger directory predicate maybeTime maybeResult dirStream =
  do nextName <- readDirStream dirStream
     logger "Next name" [shows nextName]
     if length nextName < 1
       then return maybeResult
       else
         do maybeBetterResult <-
              do if predicate nextName
                   then
                     do let path = directory </> nextName
                        modTime <- modificationTime `liftM` getFileStatus path
                        newer <- return $
                          case maybeTime of
                            Just time -> modTime > time
                            _ -> True
                        if newer
                          then searchMostRecentFile logger directory predicate (Just modTime) (Just path) dirStream
                          else return Nothing
                   else return Nothing
            case maybeBetterResult of
              Just betterResult -> return maybeBetterResult
              _ -> searchMostRecentFile logger directory predicate maybeTime maybeResult dirStream

testConcurrent :: String -> Maybe String -> Maybe String -> IO ()
testConcurrent inputFile maybeOptimizerCacheLocation maybeExportDir =
  do let wordsAlphabet = createAlphabet "words" []
         wuweiAlphabet = createAlphabet "wuwei" []
         fileAlphabet = createDirectedAlphabet "file" wuweiAlphabet wordsAlphabet
         factory = DistributionFactory $ Just histogramFromList
         pipelineContext = PipelineContext False Nothing (Just shows) (Just shows) maybeExportDir
     putStrLn ""
     putStrLn "Test concurrent LExAu.IO.ReadWordsFile"

     logVerbose "Maybe export dir" [shows maybeExportDir]
     case maybeExportDir of
       Just exportDir -> showLastTest logVerbose exportDir
       _ -> return ()

     putStrLn ""
     putStrLn "Run pipeline"
     encounterStartState <- createEncounter inputFile fileAlphabet
     stdGen <- getStdGen
     recorderStartState <- return $ createEmptyHistoryTreeProcess fileAlphabet factory stdGen
     expectedStartState <- return $ createEmptyHistoryTreeExpected fileAlphabet factory maybeOptimizerCacheLocation
     (encounterNextState, maybeRecorderNextState) <-
       runPipeline fileAlphabet encounterStartState NoPolicy recorderStartState (getUpdateSteps 1) expectedStartState pipelineContext

     return ()

testDB :: String -> Maybe String -> Maybe String -> String-> IO ()
testDB inputFile maybeOptimizerCacheLocation maybeExportDir mongoUrl =
  do let wordsAlphabet = createAlphabet "words" []
         wuweiAlphabet = createAlphabet "wuwei" []
         fileAlphabet = createDirectedAlphabet "file" wuweiAlphabet wordsAlphabet
         factory = DistributionFactory $ Just histogramFromList
     putStrLn ""
     putStrLn "Test database LExAu.IO.ReadWordsFile"
     encounterStartState <- createEncounter inputFile fileAlphabet
     recorderStateRef <- createRecorderStateRef
     dbContext <- createDbContext mongoUrl
     runRecorder Nothing encounterStartState dbContext policyUpdateMethod maybeAddSymbol recorderStateRef

-- EOF
