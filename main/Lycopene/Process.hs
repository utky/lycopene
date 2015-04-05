{-# LANGUAGE Rank2Types #-}
module Lycopene.Process where

import           Pipes
import qualified Pipes.Prelude as PP
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import           Control.Monad.Trans
import qualified System.IO as IO
import           System.Exit

import           Lycopene.Core
import qualified Lycopene.Pretty as PR
import           Lycopene.Logger

----------------------------------------------------------------------
-- | A Process which yields data originated from domain
data LycoProcess m a = Process (Producer' a (LoggerT m) ())
                     | Failure

instance (Monad m) => Functor (LycoProcess m) where
  f `fmap` (Process pa) = Process (for pa (yield . f))
  _ `fmap` Failure    = Failure

----------------------------------------------------------------------

failure :: Monad m => LycoProcess m ()
failure = Failure

-- | Data chunk to be output into STDERR and STDOUT
type Chunk = Either T.Text T.Text


-- runProcess :: Producer a m r -> Effect m r
runProcess :: (PR.Pretty a, MonadIO m) => Producer' a m () -> Effect m ()
runProcess process = process >-> prettyfy >-> chunkToString >-> outputStream

----------------------------------------------------------------------

-- | Transform pretty printable element into data chunk.
prettyfy :: (Monad m, PR.Pretty a) => Pipe a Chunk m ()
prettyfy = await >>= process where
  process = yield . Right . PR.print

chunkToString :: (Monad m) => Pipe Chunk (Either String String) m ()
chunkToString = await >>= yield . chunkToString' where
  chunkToString' (Left logStr)  = (Left . T.unpack) logStr
  chunkToString' (Right msg) = (Right . T.unpack) msg

outputStream :: (MonadIO m) => Consumer (Either String String) m ()
outputStream = consumeEither PP.stdoutLn (PP.toHandle IO.stderr)

consumeEither :: (MonadIO m) => Consumer' String m () -> Consumer' String m () -> Consumer' (Either String String) m ()
consumeEither stdoutC stderrC = do
  eitherS <- await
  case eitherS of
    (Left l)  -> (return l) >~ stderrC
    (Right r) -> (return r) >~ stdoutC

