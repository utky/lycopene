{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Lycopene.Core.Monad
    ( LycopeneT
    , LycoT
    ) where

import Control.Monad.Trans.Class
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

import           Database.Persist.Sqlite

import Lycopene.Configuration

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

type LycoExcept = ExceptT LycoError

-- ここのどこかにPersistのMonadが入っているはずで
-- runするときにConnectionを与えてm aが戻ってくるはず
newtype LycopeneT m a = LycopeneT
                      { runLycopeneT :: ConfigReader (LycoExcept m) a }

instance Monad m => Monad (LycopeneT m) where
  return a = undefined
  mma >>= f = undefined

instance MonadTrans LycopeneT where
  lift = LycopeneT . lift . lift 

type LycoT = LycopeneT

type Lycopene = LycopeneT Identity

type Lyco = Lycopene


runLycopene :: Monad m => Configuration -> LycopeneT m a -> m (Either LycoError a)
runLycopene c = runExceptT . runReaderT' . runLycopeneT where
  runReaderT' = (flip runReaderT) c


