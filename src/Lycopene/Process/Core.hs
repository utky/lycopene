{-# LANGUAGE RankNTypes #-}
module Lycopene.Process.Core
    ( ProcessM
    , ProcessR
    , Result(..)
    , Chunk(..)
    , LycoError(..)
    , runProcess
    , runProcess'
    , runDomain
    , out
    , eachP
    , debug
    , complete
    , failure
    , module Control.Monad.Trans
    ) where

import           Pipes
import qualified Pipes.Prelude as PP
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import           Control.Applicative
import           Control.Monad.Reader (ReaderT, ask)
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified System.IO as IO
import           System.Exit

import           Lycopene.Core (LycopeneT, Persist, runLycopene)
import           Lycopene.Configuration (Configuration)
import qualified Lycopene.Print as PR
import           Lycopene.Resource

----------------------------------------------------------------------
--
data LycoError = Unexpected
               | FileNotFound String -- file path
               deriving (Eq, Show)

type Trace = [LycoError]

-- | A Process which yields data originated from domain.
-- @stdin >-> Process >-> stdout & stderr@
-- Process are combinable.
-- When we combine two process as pipeline,
-- they interact with stdin and stdout
--
-- OK | NG = NG
-- NG | OK = NG
-- OK | OK = OK
-- NG | NG = NG
-- in this case, OK is a identity of binary operator (|)
--
-- > foo = do
-- >   yield a -- emit chunk(Right) return の 実体が yieldになるとかいう
-- Monad?
-- >   info b -- emit chunk(Left)
-- >   identity


--------------------------------------------

-- 
data PsIO a = PsIO
           { psIn  :: Producer a IO ()
           , psOut :: Consumer a IO ()
           , psErr :: Consumer a IO ()
           }

-- | Process IO environment with standard in/out/err
stdPsIO :: PsIO String
stdPsIO = PsIO
          { psIn = PP.stdinLn
          , psOut = PP.stdoutLn
          , psErr = PP.toHandle IO.stderr
          }

type Process' = ReaderT (PsIO String)

choiceOut :: (Monad m) => Consumer a m r -> Consumer b m r -> Consumer (Either a b) m r
choiceOut ca cb = do
  e <- await
  case e of
    Left  a -> undefined
    Right b -> undefined

--------------------------------------------

-- |
-- domainData :: Lycopene Persist a
-- runDomain domainData :: Action
data Action a = AdminAction (IO a)
              | OperAction (ReaderT Configuration IO a)

--------------------------------------------

data Result = Success | Failure Trace deriving (Eq, Show)

instance Monoid Result where
  mempty = Success
  fa@(Failure ta) `mappend` fb@(Failure tb) = Failure $ ta `mappend` tb
  _ `mappend` f@(Failure _) = f
  f@(Failure _) `mappend` _ = f
  Success `mappend` Success = Success

-- | Data chunk to be output into STDERR and STDOUT
type Chunk = Either String String

type ProcessM m = Producer Chunk m Result 

type ProcessR m = ProcessM (ReaderT Configuration m)

-- | Run a process 
runProcess :: (MonadIO m) => ProcessM m -> Effect m Result
-- runProcess process = runProcessM process >-> prettyfy >-> chunkToString >-> outputStream
runProcess process = process >-> outputStream

runProcess' :: (MonadIO m) => ProcessM m -> m Result
runProcess' = runEffect . runProcess

runDomain' :: (MonadIO m) => LycopeneT Persist a -> ReaderT Configuration m a
runDomain' l = ask >>= (liftIO . runLycopene l)

runDomain :: (MonadIO m) =>  LycopeneT Persist a -> Producer Chunk (ReaderT Configuration m) a
runDomain = lift . runDomain'


out' :: (Monad m) => (a -> Chunk) -> a -> ProcessM m
out' f x = mempty <$ (yield . f) x

out :: (Monad m, PR.Print a) => a -> ProcessM m
out = out' $ Right . PR.printA

eachP :: (Monad m, PR.Print a) => [a] -> ProcessM m
eachP xs = complete <* mapM_ out xs

debug :: (Monad m) => String -> ProcessM m
debug x = out' Left x

complete :: (Monad m) => ProcessM m
complete = return mempty 

failure :: (Monad m) => ProcessM m
failure = return $ Failure [Unexpected]

----------------------------------------------------------------------

-- | Transform pretty printable element into data chunk.
prettyfy :: (Monad m, Show a) => Pipe a Chunk m Result
prettyfy = await >>= (\a -> mempty <$ process a) where
  process = yield . Right . show

outputStream :: (MonadIO m) => Consumer (Either String String) m Result
outputStream = mempty <$ consumeEither

-- handleEither :: Either String String -> Consumer' (Either String String) m ()
-- handleEither (Right r) = (return r) >~ PP.stdoutLn
-- handleEither (Left l)  = (return l) >~ (PP.toHandle IO.stderr)

consumeEither :: (MonadIO m) => Consumer (Either String String) m Result
consumeEither = do
  let ok a = Success <$ yield a
  eitherS <- await
  case eitherS of
    -- (Left l)  -> (return l) >~ stderrC
    -- (Right r) -> (return r) >~ stdoutC
    (Left l)  -> for (ok l) (\a -> liftIO . (IO.hPutStrLn IO.stderr) $ a) >> consumeEither
    (Right r) -> for (ok r) (\a -> liftIO . putStrLn $ a) >> consumeEither
    -- (Left l)  -> do { (ok l) >-> (PP.toHandle IO.stderr); consumeEither}
    -- (Right r) -> do { (ok r) >-> (PP.stdoutLn); consumeEither}

