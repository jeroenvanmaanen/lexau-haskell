{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- |Implementation of an Encounter that reads a file.
module LExAu.IO.ReadFile (
    createEncounter,
    test
  ) where

import Control.Monad.ST (stToIO)
import Data.Char (ord)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import System.IO (Handle, IOMode(ReadMode), hGetChar, hIsEOF, openFile)
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
import LExAu.Alphabet.Directed (DirectedAlphabet, createDirectedAlphabet)
import LExAu.Alphabet.Standard (createAlphabet)
import LExAu.Distribution.Histogram (histogramFromList)
import LExAu.Model.HistoryTree(createEmptyHistoryTreeExpected, createEmptyHistoryTreeProcess, getModelOfObservedBehavior)
import LExAu.Pipeline.Concurrent (PipelineContext(..), runPipeline)
import LExAu.Utilities.Logging (Loggable(logs), logVerbose)

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
  EncounterImpl { environment :: Handle, alphabet :: alphabetType, stepCounter :: Integer }

instance (
      Alphabet alphabetType symbolType
    ) =>
    Encounter (EncounterImpl alphabetType symbolType) NoPolicy alphabetType symbolType where

  stepCount encounter = stepCounter encounter

  stepIO encounterRef =
    do EncounterImpl environmentHandle oldAlphabet oldSteps <- stToIO $ readSTRef encounterRef
       isEOF <- hIsEOF environmentHandle
       if isEOF
         then return (Nothing, Nothing, Nothing)
         else
           do responseChar <- hGetChar environmentHandle
              (maybeNewAlphabet, response) <- return $ offerWordName oldAlphabet $ charToName responseChar
              newAlphabet <- return $
                case maybeNewAlphabet of
                  Nothing -> oldAlphabet
                  Just theNewAlphabet -> theNewAlphabet
              stToIO $ writeSTRef encounterRef (EncounterImpl environmentHandle newAlphabet (oldSteps + 1))
              return (Just response, Nothing, maybeNewAlphabet)

  updateStrategyST encounterRef policyPort = return True

charToName :: Char -> String
charToName char =
  ('<' :
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
  )

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
     return $ EncounterImpl handle alphabet 1

test :: String -> Maybe String -> IO ()
test inputFile maybeOptimizerCacheLocation =
  do let wordsAlphabet = createAlphabet "words" []
         wuweiAlphabet = createAlphabet "wuwei" []
         fileAlphabet = createDirectedAlphabet "file" wuweiAlphabet wordsAlphabet
         factory = DistributionFactory $ Just histogramFromList
         pipelineContext = PipelineContext False Nothing Nothing Nothing Nothing
     putStrLn $ ""
     putStrLn $ "Test LExAu.IO.ReadFile"

     putStrLn ""
     encounterStartState <- createEncounter inputFile fileAlphabet
     stdGen <- getStdGen
     recorderStartState <- return $ createEmptyHistoryTreeProcess fileAlphabet factory stdGen
     expectedStartState <- return $ createEmptyHistoryTreeExpected fileAlphabet factory maybeOptimizerCacheLocation
     (encounterNextState, maybeRecorderNextState) <-
       runPipeline fileAlphabet encounterStartState NoPolicy recorderStartState ((:[]) . getModelOfObservedBehavior) expectedStartState pipelineContext

     return ()

-- EOF
