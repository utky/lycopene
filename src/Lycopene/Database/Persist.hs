{-# LANGUAGE RankNTypes       #-}
module Lycopene.Database.Persist where


import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Database.HDBC (IConnection, runRaw, withTransaction)

-------------------------------------------------------------------------------
-- it could be better because `unPersist fa conn` is repeated.
-- there is some misconception.


newtype Persist a = Persist { unPersist :: forall conn. IConnection conn => conn -> IO a }

instance Functor Persist where
  f `fmap` fa = Persist runner where
    runner conn = fmap f (unPersist fa conn)

instance Applicative Persist where
  pure a = Persist $ \_ -> return a
  f <*> fa = Persist runner where
    runner conn = iof <*> iofa where
      iof  = unPersist f conn
      iofa = unPersist fa conn

instance Monad Persist where
  return = pure
  fa >>= f = Persist runner where
    ffb = f `fmap` fa
    runner conn = unwrap ffb >>= unwrap where
      unwrap = flip unPersist conn

-- | Run database middleware and gain result with side-effect.
runPersist :: ( IConnection conn , MonadIO m) => Persist r -> conn -> m r
runPersist p conn = liftIO (withTransaction conn (unPersist p))

rawPersist :: String -> Persist ()
rawPersist sql = Persist (\conn -> runRaw conn sql)

