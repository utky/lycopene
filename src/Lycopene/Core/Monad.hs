{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Lycopene.Core.Monad
    ( LycopeneT
    , LycoApp
    , runLycopeneT
    , getConfig
    , liftL
    ) where

import           Control.Monad.Trans
import           Control.Monad.Reader


import           Lycopene.Configuration


data LycoError = ValidationFailure String
               | PersistentError String
               deriving Show


type ConfigReader = ReaderT Configuration

--type LycoExcept = ExceptT LycoError

type LycopeneT = ConfigReader

type LycoApp = LycopeneT IO

runLycopeneT :: LycopeneT m a -> Configuration -> m a
runLycopeneT = runReaderT

liftL :: Monad m => m a -> LycopeneT m a
liftL = lift

getConfig :: Monad m => LycopeneT m Configuration
getConfig = ask

