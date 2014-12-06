{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Core.Persist where 

import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Database.Persist.Sqlite
import           Data.Text
import           System.FilePath
import           Control.Monad.Identity

import           Lycopene.Core.Entity
import           Lycopene.Core.Monad
import           Lycopene.Configuration (Configuration(..))

{- | Repository is a container of entity which have behavior like list.
-}
-- type RepositoryT m a = Source m a
-- type Repository a = Source Identity a


data Manipulate a = InsertEntity a
                  | UpdateEntity (Key a) [Update a]
                  | DeleteEntity (Key a)



datapath :: Configuration -> Text
datapath config = pack path where
  -- path = lycoHome config </> "issues.db"
  path = lycoHome config


initDatabase cfg = runSqlite (datapath cfg) $ do
  runMigrationSilent migrateAll

runDB m = runSqlite (datapath cfg) $ do
  initDatabase cfg
  m
  where
    --dpath = datapath cfg
    cfg = memory

memory = Configuration ":memory:"

{-
-- あー変に中に構造を抱え込もうとするからダメなのかなー。
-- なんか、こう、それは別途外部からいただくという設計にしていくのがいいのかな
-- ただ、もうらうとしてそれをどういう型になるのだろう
-- どう考えてもPersistのAPI Monadに依存した型が出てくる気がす
--
-- それが伝播するのを防止するためにには、
-- 別のMonadでそのあたりの評価を遅延することはできるか
--
-- いや、でもどっかでその文脈はキープすることになるんだよな、
-- 内部的にPersistの関数に依存している限り
-}

