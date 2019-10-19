
module LExAu.Model.HistoryTreeDBImpl.Habits
  ( policyUpdateMethod
  ) where

import LExAu.API.MongoDB(DBContext)

policyUpdateMethod ::
    ( DBContext dbContextType
    ) =>
  dbContextType -> IO (Maybe policyType)

policyUpdateMethod dbContext = return Nothing
