{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           Data.Time
import           Database.Persist.TH

import qualified Data.Text           as T

share [mkPersist sqlSettings, mkMigrate "migrateModel"] [persistLowerCase|
Customer json
     name T.Text
     deriving Show
Item json
     name T.Text
     price Double
     revenue Double
     deriving Show
Sale json
     item ItemId
     customer CustomerId
     amount Int
     time UTCTime
     deriving Show
|]
