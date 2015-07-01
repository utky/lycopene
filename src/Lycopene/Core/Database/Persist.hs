{-# LANGUAGE RankNTypes       #-}
module Lycopene.Core.Database.Persist where


import           Control.Monad.IO.Class (liftIO)
import           Database.HDBC

import           Lycopene.Core.Monad
import           Lycopene.Core.Context
import           Lycopene.Core.Database.DataSource (withDataSource)

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

-- instance MonadIO Persist where
--   liftIO ia = Persist $ \_ -> ia

runPersist :: Persist a -> Lycopene a
runPersist p = do
  ctx <- context
  liftIO $ withDataSource (dataSource ctx) (unPersist p)
-------------------------------------------------------------------------------

direct :: (forall conn. IConnection conn => conn -> IO a) -> Persist a
direct = Persist

