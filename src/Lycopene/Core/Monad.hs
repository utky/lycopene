{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Lycopene.Core.Monad
    ( LycopeneT
    , runLycopeneT
    , getConfig
    ) where

import           Control.Monad.Reader
--import           Control.Monad.Except


import           Lycopene.Configuration

-- | First intuitive definition of runner

{-
-- start :: Configuration -> Session
-- run :: Configuration -> a -> either b
--   runReaderT :: Configuration -> ExceptT m error a
--   runExceptT :: ma -> m (Either error a)
--   Configuration -> 
-}

data LycoError = ValidationFailure String
               | PersistentError String
               deriving Show

{-
-- Eitherを使って「失敗するかもしれない」可能性を相手に伝えるのは必須だなあ
-- もっとも単純な表現
-}

type ConfigReader = ReaderT Configuration

--type LycoExcept = ExceptT LycoError

-- ここのどこかにPersistのMonadが入っているはずで
-- runするときにConnectionを与えてm aが戻ってくるはず
type LycopeneT = ConfigReader


runLycopeneT :: LycopeneT m a -> Configuration -> m a
runLycopeneT = runReaderT

getConfig :: Monad m => LycopeneT m Configuration
getConfig = ask
