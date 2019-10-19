-- |Logging facilities.
module LExAu.Utilities.Logging (
    Level(..),
    LogChannel,
    LogFunction,
    Loggable(logs),
    Logger,
    ShowRef(ShowRef),
    Summarize(summarize),
    WrapShowS(WrapShowS),
    createLogWriter,
    logFunction,
    logSilent,
    logListVertically,
    logVerbose,
    showListVertically
  ) where

import Control.Concurrent.Chan(Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, try)
import Data.Foldable (Foldable)
import Data.PrettyPrint.Swallow (prettyPrint)
import Data.Sequence (Seq)
import Data.Time.Clock (UTCTime(utctDayTime), getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import System.IO (Handle, hFlush, hPutStr, hPutStrLn)
import System.Locale (defaultTimeLocale)
import Text.Show (showListWith)
import qualified Data.Foldable as Fold (toList)

-- Taken literally from java.util.logging
data Level = SEVERE | WARNING | INFO | CONFIG | FINE | FINER | FINEST deriving Show

type LogFunction = String -> [ShowS] -> IO ()
newtype Logger = Logger (Level -> String -> LogFunction)

newtype LogChannel = LogChannel (Chan (String, Level, String, String, [ShowS]))

data ShowRef refType = ShowRef refType

data WrapShowS = WrapShowS ShowS 
instance Show WrapShowS where
  showsPrec _ (WrapShowS theShowS) = theShowS

class Loggable loggableType where
  logs :: loggableType -> ShowS

class Summarize summarizeType where
  summarize :: summarizeType -> ShowS

instance (Loggable refType) => Show (ShowRef refType) where
  showsPrec _ (ShowRef reference) = logs reference

instance (Loggable justType) => Loggable (Maybe justType) where
  logs Nothing = showString "Nothing"
  logs (Just loggable) = showString "Just " . logs loggable

instance (Loggable itemType) => Loggable [itemType] where
  logs loggableList = logListVertically loggableList

instance (Loggable itemType) => Loggable (Seq itemType) where
  logs loggableSeq = logListVertically $ Fold.toList loggableSeq

-- |Does nothing.
logSilent :: LogFunction
logSilent _ _ = do return ()

-- |Logs the given objects with the given label.
logVerbose :: LogFunction
logVerbose label showFunctions =
  let showLine = formatLog label showFunctions
      line = showLine ""
  in do result <- (try $ putStr $ prettyPrint 60 2 line "\n") :: IO (Either SomeException ())
        case result of
          Right a -> return ()
          Left exception -> putStrLn ("WRONG SYNTAX: " ++ (shows exception "") ++ ": " ++ line)

createLogWriter :: Handle -> IO (String -> Logger, IO ())
createLogWriter handle =
  do let logWriter channel =
           do (source, level, category, label, showFunctions) <- readLog channel
              utcTime <- getCurrentTime
              (_, subsecond) <- return $ properFraction $ toRational $ utctDayTime utcTime
              millis <- return $ floor (1000 * subsecond)
              fMillis <- return $ drop 1 $ show (1000 + millis)
              localTime <- utcToLocalZonedTime utcTime
              longLabelOrError <- try $ return $!
                ( showString (formatTime defaultTimeLocale "%T" localTime)
                . showString "."
                . showString fMillis
                . showString ": "
                . showString source
                . showString ": "
                . shows level
                . showString ": "
                . showString category
                . showString ": "
                . showString label
                ) ""
              case longLabelOrError of
                Left error ->
                  hPutStrLn handle $ "EXCEPTION WHILE FORMATTING DETAILS: " ++ (shows (error :: SomeException) "")
                Right longLabel ->
                  do result <- try $ return $! formatLog longLabel showFunctions ""
                     either (\ e -> hPutStrLn handle (longLabel ++ ": EXCEPTION WHILE FORMATTING DETAILS: " ++ (shows (e :: SomeException) ""))) (\ a -> printLogLine handle a) result
              hFlush handle
              logWriter channel
     logChannel <- (LogChannel `fmap` newChan) :: IO LogChannel
     return (\ source -> Logger $ sendLog logChannel source, logWriter logChannel)

printLogLine :: Handle -> String -> IO ()

printLogLine handle line =
  do result <- (try $ hPutStr handle $ prettyPrint 120 2 line "\n") :: IO (Either SomeException ())
     either (\e -> hPutStrLn handle ("WRONG SYNTAX: " ++ line)) (\a -> return ()) result

readLog :: LogChannel -> IO (String, Level, String, String, [ShowS])
readLog (LogChannel channel) = readChan channel

-- |Sends the log information to the given channel
sendLog :: LogChannel -> String -> Level -> String -> LogFunction
sendLog (LogChannel channel) source level category label showFunctions =
  writeChan channel (source, level, category, label, showFunctions)

logFunction :: Level -> Logger -> (String -> LogFunction)
logFunction level (Logger logger) = logger level

formatLog :: String -> [ShowS] -> ShowS
formatLog label showFunctions =
  foldr (.) id ((showString label) : (map (\f -> showString ": " . f) showFunctions))

showListVertically :: (Show aType) => [aType] -> ShowS
showListVertically items = showListWith (\a -> showString "\n  " . shows a)  items

logListVertically :: (Loggable aType) => [aType] -> ShowS
logListVertically items = showListWith (\a -> showString "\n  " . logs a)  items

-- EOF
