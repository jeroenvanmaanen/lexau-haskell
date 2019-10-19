{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module LExAu.Model.HistoryTreeImpl.Utilities
  ( LogEntry(logEntry)
  , modifiedChar
  ) where

import LExAu.API.HistoryTree
  ( UpdateMarkers
  )
import LExAu.API.Indexed (Indexed)

class (Indexed alphabetType symbolType Int) => LogEntry entryType alphabetType symbolType where
  logEntry :: alphabetType -> (Maybe UpdateMarkers) -> String -> (Int, entryType) -> ShowS

modifiedChar :: (Maybe UpdateMarkers) -> Char
modifiedChar (Just _) = '*'
modifiedChar Nothing = ' '
