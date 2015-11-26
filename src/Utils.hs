{-# LANGUAGE TypeFamilies #-}
module Utils where

import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Database.Persist.Sql         (SqlBackend, SqlPersistT,
                                               runSqlConn)
import           Web.Spock.Shared

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn
{-# INLINE runSQL #-}
