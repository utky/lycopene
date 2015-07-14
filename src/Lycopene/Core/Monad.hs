module Lycopene.Core.Monad
    ( Lycopene(..)
    , LycoError(..)
    , context
    , runLycopene
    ) where

import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.Except

import           Lycopene.Core.Context

data LycoError = ValidationFailure !String
               | PersistentError !String
               deriving (Show, Eq)

newtype Lycopene a = Lyco { unLyco :: ReaderT Context (ExceptT LycoError IO) a }

instance Functor Lycopene where
  fmap f x = Lyco $ fmap f $ unLyco x

instance Applicative Lycopene where
  pure = Lyco . lift . return
  f <*> x = Lyco $ unLyco f <*> unLyco x

instance Monad Lycopene where
  return = pure
  x >>= k = Lyco $ (unLyco x) >>= unLyco . k

instance MonadIO Lycopene where
  liftIO = Lyco . lift . liftIO

context :: Lycopene Context
context = Lyco ask

runLycopene :: Lycopene a -> Context -> IO (Either LycoError a)
runLycopene lyco ctx = runExceptT (runReaderT (unLyco lyco) ctx)

