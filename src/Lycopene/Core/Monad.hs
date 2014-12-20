{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Lycopene.Core.Monad
    ( LycopeneT
    , LycoApp
    , runLycopeneT
    , getConfig
    ) where

import           Control.Monad.Reader
import           Control.Monad.Logger
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

-- xcX1Hq3B

type ConfigReader = ReaderT Configuration

--type LycoExcept = ExceptT LycoError

type LycopeneT = ConfigReader

type LycoApp = LycopeneT IO

runLycopeneT :: LycopeneT m a -> Configuration -> m a
runLycopeneT = runReaderT


getConfig :: Monad m => LycopeneT m Configuration
getConfig = ask
