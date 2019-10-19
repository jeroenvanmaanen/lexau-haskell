{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module LExAu.API.ReaderContext (ReaderContext(readsWithContext)) where

-- |Method to read data using (types of) factories to create (or determine types
-- of) parts of the result.
class ReaderContext factoriesType resultType | factoriesType -> resultType where

  -- | Creates a reader from the given the necessary factories.
  readsWithContext :: factoriesType -> ReadS resultType
