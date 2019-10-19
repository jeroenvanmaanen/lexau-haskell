{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module LExAu.API.MongoDB
  ( DBContext(..)
  ) where

import Data.Sequence (Seq)

class DBContext dbContextType where
  ensureNode :: dbContextType -> Seq Int -> IO Bool
  incrementNodeSymbol :: dbContextType -> Seq Int -> Int -> IO ()

-- EOF