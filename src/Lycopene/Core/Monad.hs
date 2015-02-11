{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Lycopene.Core.Monad
    ( LycopeneT
    , runLycopeneT
    , config
    , liftL
    ) where

import           Control.Monad.Trans
import           Control.Monad.Reader


import           Lycopene.Configuration

data LycoError = ValidationFailure String
               | PersistentError String
               deriving Show



type ConfigReader = ReaderT Configuration

type LycopeneT = ConfigReader

runLycopeneT :: Monad m => LycopeneT m a -> Configuration -> m a
runLycopeneT = runReaderT


-------------------------------------------------------------------------------
-- Monadic Operations

liftL :: Monad m => m a -> LycopeneT m a
liftL = lift

config :: Monad m => LycopeneT m Configuration
config = ask

