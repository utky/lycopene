{-# LANGUAGE Rank2Types #-}
module Lycopene.Process
    ( LycoError(..)
    , Result(..)
    , Chunk
    , ProcessM
    , runProcess
    , out
    , debug
    ) where

import           Pipes
import qualified Pipes.Prelude as PP
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import           Control.Applicative
import           Control.Monad.Trans
import qualified System.IO as IO
import           System.Exit

import           Lycopene.Core
import qualified Lycopene.Pretty as PR

----------------------------------------------------------------------
--
data LycoError = Unexpected
               | FileNotFound String -- file path

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

data Result = Success | Failure Trace

instance Monoid Result where
  mempty = Success
  fa@(Failure ta) `mappend` fb@(Failure tb) = Failure $ ta `mappend` tb
  _ `mappend` f@(Failure _) = f
  f@(Failure _) `mappend` _ = f
  Success `mappend` Success = Success

-- | Data chunk to be output into STDERR and STDOUT
type Chunk = Either T.Text T.Text

newtype ProcessM m = Process { runProcessM :: (MonadIO m) => Producer' Chunk m Result }

-- | Run a process 
runProcess :: (MonadIO m) => ProcessM m -> Effect m Result
-- runProcess process = runProcessM process >-> prettyfy >-> chunkToString >-> outputStream
runProcess process = runProcessM process >-> chunkToString >-> outputStream

out' :: (MonadIO m) => (a -> Chunk) -> a -> ProcessM m
out' f x = Process $ mempty <$ (yield . f) x

out :: (MonadIO m, PR.Pretty a) => a -> ProcessM m
out = out' $ Right . PR.print

debug :: (MonadIO m) => T.Text -> ProcessM m
debug x = out' Left x

----------------------------------------------------------------------

-- | Transform pretty printable element into data chunk.
prettyfy :: (Monad m, PR.Pretty a) => Pipe a Chunk m Result
prettyfy = await >>= (\a -> mempty <$ process a) where
  process = yield . Right . PR.print

chunkToString :: (Monad m) => Pipe Chunk (Either String String) m Result
chunkToString = mempty <$ (await >>= yield . chunkToString') where
  chunkToString' (Left logStr)  = (Left . T.unpack) logStr
  chunkToString' (Right msg) = (Right . T.unpack) msg

outputStream :: (MonadIO m) => Consumer (Either String String) m Result
outputStream = mempty <$ consumeEither PP.stdoutLn (PP.toHandle IO.stderr)

consumeEither :: (MonadIO m) => Consumer' String m r -> Consumer' String m r -> Consumer' (Either String String) m r
consumeEither stdoutC stderrC = do
  eitherS <- await
  case eitherS of
    (Left l)  -> (return l) >~ stderrC
    (Right r) -> (return r) >~ stdoutC

