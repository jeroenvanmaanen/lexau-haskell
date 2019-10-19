-- |Main module for the LExAu executable.
module Main (main) where

import Control.Exception (bracket)
import Data.Monoid (Monoid(mappend,mempty))
import Distribution.ReadE(ReadE(ReadE,runReadE))
import Distribution.Simple.Command (
    CommandParse(CommandReadyToGo),
    OptDescr(BoolOpt),
    OptionField(OptionField),
    commandAddAction,
    commandUsage,
    commandsRun,
    makeCommand,
    optArg,
    optArg'
  )
import Distribution.Simple.Setup (Flag(Flag,NoFlag), flagToMaybe, fromFlag)
import Numeric (readDec)
import System.Environment (getArgs)

import LExAu.Utilities.Logging(logVerbose)
import LExAu.Utilities.ThreadManager (forkManaged, newManager, stopLogger, waitAll)
import qualified LExAu.Alphabet.Standard as Alphabet (test)
import qualified LExAu.Distribution.Histogram as Histogram (test)
import qualified LExAu.Distribution.MDL as MDL (precompute, test)
import qualified LExAu.IO.Example as Example (test)
import qualified LExAu.IO.ReadFile as ReadFile (test)
import qualified LExAu.IO.ReadWordsFile as ReadWordsFile (testConcurrent, testDB)
import qualified LExAu.Model.HistoryTree as HistoryTree (test)
import qualified LExAu.Model.Markov as Markov (test)
import qualified LExAu.Model.Trivial as Trivial (test)
import qualified LExAu.Pipeline.Concurrent as Concurrent (test)
import qualified LExAu.Utilities.MongoDB as MongoDB (test)
import qualified LExAu.Utilities.DescriptionLength as DescriptionLength (test)

data LExAuFlags =
  LExAuFlags
    { lexauSingleTest :: Flag Bool
    , lexauEncounterOnly :: Flag Bool
    , lexauStepLimit :: Flag Integer
    , lexauRunForever :: Flag Bool
    , lexauInputFile :: Flag String
    , lexauPrecompute :: Flag Bool
    , lexauOracleFile :: Flag String
    , lexauExportDir :: Flag String
    , lexauMongoUrl :: Flag String
    }

-- Boilerplate monoid instance.
instance Monoid LExAuFlags where
  mempty = LExAuFlags
    { lexauSingleTest = mempty
    , lexauEncounterOnly = mempty
    , lexauStepLimit = mempty
    , lexauRunForever = mempty
    , lexauInputFile = mempty
    , lexauPrecompute = mempty
    , lexauOracleFile = mempty
    , lexauExportDir = mempty
    , lexauMongoUrl = mempty
    }
  mappend a b = LExAuFlags
    { lexauSingleTest = mult lexauSingleTest
    , lexauEncounterOnly = mult lexauEncounterOnly
    , lexauStepLimit = mult lexauStepLimit
    , lexauRunForever = mult lexauRunForever
    , lexauInputFile = mult lexauInputFile
    , lexauPrecompute = mult lexauPrecompute
    , lexauOracleFile = mult lexauOracleFile
    , lexauExportDir = mult lexauExportDir
    , lexauMongoUrl = mult lexauMongoUrl
    } where mult f = f a `mappend` f b

-- OptArg Description OptFlags ArgPlaceHolder (ReadE (a -> a)) (a -> a) (a -> [Maybe String])
runReadNumE :: (Num numType) => String -> Either String (Flag numType)
runReadNumE s =
  case readDec s of
    [(value, "")] -> (Right $ Flag value)
    _ -> (Left $ "Not an integer: '" ++ s ++ "'")

-- |Executes all tests.
main::IO ()
main =
  do let defaultFlags = (LExAuFlags (Flag False) (Flag False) (Flag 100000) (Flag False) (Flag "README") (Flag False) (Flag "lexau-oracle.data") NoFlag (Flag "localhost/lexau"))
         lexauCommandUI =
           makeCommand
             "lexau"
             "LExAu: Learning Expectations Autonomously"
             Nothing
             defaultFlags
             (\_ -> [
               OptionField
                 "single-test"
                 [BoolOpt
                   "Execute just one single test"
                   ("s", ["single-test"]) ("c", ["complete-test-suite"])
                   (\flag flags -> flags { lexauSingleTest = Flag flag })
                   (flagToMaybe . lexauSingleTest)
                 ],
               OptionField
                 "encounter-only"
                 [BoolOpt
                   "Run only the encounter thread"
                   ("e", ["encounter-only"]) ("a", ["all-threads"])
                   (\flag flags -> flags { lexauEncounterOnly = Flag flag })
                   (flagToMaybe . lexauEncounterOnly)
                 ],
               OptionField
                 "run-forever"
                 [BoolOpt
                   "Continue to run the threads forever"
                   ("f", ["run-forever"]) ("l", ["limit-run"])
                   (\flag flags -> flags { lexauRunForever = Flag flag })
                   (flagToMaybe . lexauRunForever)
                 ],
               OptionField
                 "step-limit"
                 [optArg
                   "limit"
                   (ReadE runReadNumE)
                   NoFlag
                   (\flag -> case flagToMaybe flag of Nothing -> [] ; (Just int) -> [Just $ show int])
                   "m"
                   ["step-limit"]
                   "The maximum number of steps"
                   lexauStepLimit
                   (\flag flags -> flags { lexauStepLimit = flag })
                 ],
               OptionField
                 "input-file"
                 [optArg'
                   "input-file"
                   (\maybeArg -> case maybeArg of Nothing -> NoFlag ; Just arg -> Flag arg)
                   (\flag -> case flagToMaybe flag of Nothing -> [] ; (Just file) -> [Just file])
                   "i"
                   ["input-file"]
                   "The input file for the ReadFile encounter"
                   lexauInputFile
                   (\flag flags -> flags { lexauInputFile = flag })
                 ],
               OptionField
                 "precompute"
                 [BoolOpt
                   "Precompute interval boundaries for the MDL optimizer"
                   ("p", ["precompute"]) ("r", ["run"])
                   (\flag flags -> flags { lexauPrecompute = Flag flag })
                   (flagToMaybe . lexauPrecompute)
                 ],
               OptionField
                 "oracle-file"
                 [optArg'
                   "oracle-file"
                   (\maybeArg -> case maybeArg of Nothing -> NoFlag ; Just arg -> Flag arg)
                   (\flag -> case flagToMaybe flag of Nothing -> [] ; (Just file) -> [Just file])
                   "o"
                   ["oracle-file"]
                   "The file that contains the oracle data"
                   lexauOracleFile
                   (\flag flags -> flags { lexauOracleFile = flag })
                 ],
               OptionField
                 "export-dir"
                 [optArg'
                   "export-dir"
                   (\maybeArg -> case maybeArg of Nothing -> NoFlag ; Just arg -> Flag arg)
                   (\flag -> case flagToMaybe flag of Nothing -> [] ; (Just dir) -> [Just dir])
                   "d"
                   ["export-dir"]
                   "The directory that is used to store exported data"
                   lexauExportDir
                   (\flag flags -> flags { lexauExportDir = flag })
                 ],
               OptionField
                 "mongo-url"
                 [optArg'
                   "mongo-url"
                   (\maybeArg -> case maybeArg of Nothing -> NoFlag ; Just arg -> Flag arg)
                   (\flag -> case flagToMaybe flag of Nothing -> [] ; (Just dir) -> [Just dir])
                   "b"
                   ["mongo-url"]
                   "The URL that is used to access the MongoDB database"
                   lexauMongoUrl
                   (\flag flags -> flags { lexauMongoUrl = flag })
                 ]
               ]
             )
         runCommand = commandAddAction lexauCommandUI (\_ _ -> ())

     args <- getArgs
     putStrLn $ commandUsage lexauCommandUI "" 
     commandFlags <- return $ commandsRun lexauCommandUI [runCommand] args
     flags <- do
       case commandFlags of
         (CommandReadyToGo (theFlags, _)) ->
           do return $ theFlags
         otherwise ->
           do putStrLn $ commandUsage lexauCommandUI ""
              error "Exit"
     logVerbose "Single test" [shows $ lexauSingleTest flags]
     logVerbose "Encounter only" [shows $ lexauEncounterOnly flags]
     logVerbose "Run forever" [shows $ lexauRunForever flags]
     logVerbose "Step limit" [shows $ lexauStepLimit flags]
     logVerbose "Input file" [shows $ lexauInputFile flags]
     logVerbose "Precompute" [shows $ lexauPrecompute flags]
     logVerbose "Oracle file" [shows $ lexauOracleFile flags]
     logVerbose "Export directory" [shows $ lexauExportDir flags]
     logVerbose "Mongo URL" [shows $ lexauMongoUrl flags]
     logVerbose "End of flags" []

     if fromFlag $ lexauPrecompute flags
       then
         do MDL.test (Just $ fromFlag (lexauOracleFile flags))
            MDL.precompute (fromFlag $ lexauOracleFile flags)
       else
         do if fromFlag $ lexauSingleTest flags
              then return ()
              else
                do logVerbose "First all preliminary tests" []
                   bracket
                     newManager
                     stopLogger
                     (\ threadManager ->
                       do threadId <- forkManaged threadManager $
                            foldl (>>) (return ()) (take 10 $ repeat $ putStr "Hello\n")
                          foldl (>>) (return ()) (take 10 $ repeat $ putStr "  World\n")
                          waitAll threadManager
                     )

                   Alphabet.test
                   Histogram.test
                   Trivial.test
                   DescriptionLength.test
                   Markov.test
                   Example.test
                   HistoryTree.test
                   Concurrent.test
                      (fromFlag $ lexauEncounterOnly flags)
                      (case fromFlag (lexauRunForever flags) of True -> Nothing ; otherwise -> Just $ fromFlag (lexauStepLimit flags))
                      (Just $ fromFlag (lexauOracleFile flags))
                   ReadFile.test (fromFlag $ lexauInputFile flags) (Just $ fromFlag (lexauOracleFile flags))
                   ReadWordsFile.testConcurrent (fromFlag $ lexauInputFile flags) (Just $ fromFlag (lexauOracleFile flags)) (flagToMaybe $ lexauExportDir flags)
            logVerbose "Main test" []
            MongoDB.test (fromFlag $ lexauMongoUrl flags)
            ReadWordsFile.testDB (fromFlag $ lexauInputFile flags) (Just $ fromFlag (lexauOracleFile flags)) (flagToMaybe $ lexauExportDir flags) (fromFlag $ lexauMongoUrl flags)

-- EOF
