module Lycopene.Core.Resource
    ( Resource
    , runResource
    , file
    , connection
    ) where

import           System.IO
import           Database.HDBC (IConnection, disconnect)
import           Control.Monad.Trans
import           Control.Exception (bracket, onException)


-- | Generic Resource
-- http://www.haskellforall.com/2013/06/the-resource-applicative.html
newtype Resource a = Resource { acquire :: IO (a, IO ()) }

runResource :: Resource a -> (a -> IO b) -> IO b
runResource resource k = bracket (acquire resource)
                                 snd
                                 (\(a, _) -> k a)

instance Functor Resource where
    fmap f resource = Resource $ do
        (a, release) <- acquire resource
        return (f a, release)

instance Applicative Resource where
    pure a = Resource (pure (a, pure ()))
    resource1 <*> resource2 = Resource $ do
        (f, release1) <- acquire resource1
        (x, release2) <- acquire resource2 `onException` release1
        return (f x, release2 >> release1)

instance Monad Resource where
    return a = Resource (return (a, return ()))
    m >>= f = Resource $ do
        (m', release1) <- acquire m
        (x , release2) <- acquire (f m') `onException` release1
        return (x, release2 >> release1)

instance MonadIO Resource where
    liftIO m = Resource $ m >>= (\x -> return (x, return ()))

file :: IOMode -> FilePath -> Resource Handle
file mode path = Resource $ do
    handle <- openFile path mode
    return (handle, hClose handle)

connection :: (IConnection conn) => IO conn -> Resource conn
connection c = Resource $ c >>= (\x -> return (x, disconnect x))
