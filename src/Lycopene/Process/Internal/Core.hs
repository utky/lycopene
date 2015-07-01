{-# LANGUAGE RankNTypes #-}
module Lycopene.Process.Internal.Core 
  ( Process
  , ProcessEnv(..)
  , runWriterPs
  , runPure
  , handleOut
  , each'
  , left'
  , yieldWriter
  , printer
  , choice
  , debug
  , info
  , warn
  , error
  , fatal
  , module Pipes
  ) where

import           Prelude hiding (error)
import qualified Data.Text as T
import           Pipes
import qualified Pipes.Prelude as P
import           Pipes.Lift (runWriterP)
import qualified Control.Monad.Writer.Strict as W
import           Control.Exception (throwIO, try)
import qualified GHC.IO.Exception as G
import           Foreign.C.Error (Errno(Errno), ePIPE)
import qualified System.IO as IO
import qualified Lycopene.Logger as L
import qualified Lycopene.Print as PR

----------------------------------------------------------------------

type Process a m r = Producer a (L.LoggerT m) r

data ProcessEnv = ProcessEnv
                { hOut :: IO.Handle
                , hErr :: IO.Handle
                , hIn :: IO.Handle
                }

handleOut :: (MonadIO m) => IO.Handle -> Consumer String m ()
handleOut h = go where
  go = do
    str <- await
    x   <- liftIO $ try (IO.hPutStrLn h str)
    case x of
      Left (G.IOError { G.ioe_type  = G.ResourceVanished
                      , G.ioe_errno = Just ioe })
           | Errno ioe == ePIPE
             -> return ()
      Left  e  -> liftIO (throwIO e)
      Right () -> go

each' :: (Monad m, Foldable f) => Pipe (f a) a m ()
each' = await >>= each

yieldWriter
  :: (Monad m, Monoid w)
  => Proxy a' a () b (W.WriterT w m) r
  -> Proxy a' a () (Either w b) m r
yieldWriter p = let right = P.map Right
                    wp    = runWriterP p
                    writtenP = fmap snd wp
                    valueP = fmap fst wp
                in (writtenP >-> P.drain) >>= (yield . Left) >> (valueP >-> right)

printer :: (Monad m, PR.Print a) => Pipe a String m ()
printer = P.map PR.printA

eitherPrinter :: (Monad m, PR.Print a, PR.Print b) => Pipe (Either a b) (Either String String) m ()
eitherPrinter = P.map (either (Left . PR.printA) (Right . PR.printA))

choice :: (Monad m) =>
          Consumer String m () -- Left type consumer
       -> Consumer String m () -- Right type consumer
       -> Consumer (Either String String) m ()
choice lc rc = do
  let consume p c = (yield p) >-> c >> choice lc rc
  e <- await
  case e of
    (Left l)  -> consume l lc
    (Right r) -> consume r rc

eachLeft :: (Monad m, Foldable f) => Pipe (Either (f a) b) (Either a b) m ()
eachLeft = do
  e <- await
  case e of
    Left l -> (each l) >-> (P.map Left)
    Right r -> yield (Right r)

left' :: (Monad m) => (a -> b) -> Pipe (Either a c) (Either b c) m ()
left' f = P.map (either (Left . f) Right)

runWriterPs
  :: (Monad m, Foldable f, Monoid (f a), PR.Print b, PR.Print a) =>
     Producer b (W.WriterT (f a) m) ()
     -> Producer (Either String String) m ()
runWriterPs p = yieldWriter p >-> eachLeft >-> eitherPrinter

runPure :: (Monad m) => Producer (Either String String) m () -> m [Either String String]
-- runPure p = P.toListM $ yieldWriter p >-> eachLeft >-> left' PR.printA
runPure p = P.toListM $ p >-> eitherPrinter

debug :: (Monad m) => String -> Process a m ()
debug = lift . L.debug . T.pack

info :: (Monad m) => String -> Process a m ()
info = lift . L.info . T.pack

warn :: (Monad m) => String -> Process a m ()
warn = lift . L.warn. T.pack

error :: (Monad m) => String -> Process a m ()
error = lift . L.error. T.pack

fatal :: (Monad m) => String -> Process a m ()
fatal = lift . L.fatal. T.pack
