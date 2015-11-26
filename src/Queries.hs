module Queries where

import           Model

import           Control.Monad
import           Control.Monad.Trans
import           Database.Esqueleto

getTotalRevenue :: MonadIO m => SqlPersistT m [Maybe Double]
getTotalRevenue =
    liftM (map unValue) $
    select $
    from $ \(s, i, c) ->
    do where_ (s ^. SaleItem ==. i ^. ItemId &&. s ^. SaleCustomer ==. c ^. CustomerId)
       return (sum_ $ castNum (s ^. SaleAmount) *. i ^. ItemRevenue)
