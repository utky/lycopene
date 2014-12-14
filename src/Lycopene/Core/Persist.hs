{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Core.Persist
    ( runDB
    , LycoDB
    ) where 

import           Control.Monad.Reader
import           Database.Persist.Sqlite
import           Data.Text
import           System.FilePath

import           Lycopene.Core.Monad
import           Lycopene.Configuration (Configuration(..))


datapath :: Configuration -> Text
datapath config = pack path where
  path = lycoHome config </> "issues.db"

type LycoDB = SqlPersistM

runDB :: LycoDB a -> LycopeneT IO a
runDB db = do
    cfg <- getConfig
    let dp = datapath cfg
    lift $ runSqlite dp db

