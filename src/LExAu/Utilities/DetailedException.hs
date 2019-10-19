{-# LANGUAGE DeriveDataTypeable #-}
module LExAu.Utilities.DetailedException (DetailedException(DetailedException), addDetail, enrich) where

import Data.Typeable (cast)
import Control.Exception (Exception, NonTermination(NonTermination), SomeException(SomeException), mapException)
import Data.Typeable (Typeable)

data DetailedException = DetailedException { theCause :: SomeException, theDetails :: ShowS } deriving Typeable

instance Exception DetailedException

instance Show DetailedException where
  showsPrec _ (DetailedException cause details) = showString "DetailedException " . shows cause . showChar ' ' . showString (details "")

addDetail :: ShowS -> DetailedException -> DetailedException
addDetail detail exception = exception { theDetails = (theDetails exception) . showString ": " . detail }

enrich :: ShowS -> a -> a
enrich newDetails =
  mapException (\ex ->
    case cast (ex, DetailedException (SomeException NonTermination) id) of
      Just detailedException -> addDetail newDetails detailedException
      Nothing -> DetailedException ex newDetails
  )

