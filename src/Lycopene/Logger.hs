module Lycopene.Logger
    ( Log
    , LogMessage (..)
    , LogLevel (..)
    , formatLog
    , LoggerT
    , runLoggerT
    , execLoggerT
    , debug
    , info
    , warn
    , error
    , fatal
    , log
    ) where

import Prelude hiding (log, error)
import qualified Data.Text as T
import qualified Control.Monad.Writer as W

-----------------------------------------------------------------------------

data LogLevel = Debug | Info | Warning | Error | Fatal deriving (Eq, Ord)

instance Show LogLevel where
  show Debug = "[DEBUG]"
  show Info  = "[INFO]"
  show Warning = "[WARN]"
  show Error  = "[ERROR]"
  show Fatal = "[FATAL]"


-----------------------------------------------------------------------------

data LogMessage = LogMessage LogLevel T.Text

instance Show LogMessage where
  show (LogMessage l t) = (show l) ++ " " ++ (T.unpack t)

-----------------------------------------------------------------------------

type Log = [LogMessage]

formatLog :: Log -> [T.Text]
formatLog ls = map formatEntry ls where
  formatEntry (LogMessage lvl msg) = T.concat [T.pack $ show lvl, T.pack " ", msg]

-----------------------------------------------------------------------------

type LoggerT = W.WriterT Log

runLoggerT :: (Monad m) => LoggerT m a -> m (a, Log)
runLoggerT = W.runWriterT

execLoggerT :: (Monad m) => LoggerT m a -> m Log
execLoggerT = W.execWriterT

-----------------------------------------------------------------------------

log :: (Monad m) => LogLevel -> T.Text -> LoggerT m ()
log l t = W.tell . return $ LogMessage l t

debug :: (Monad m) => T.Text -> LoggerT m ()
debug = log Debug

info :: (Monad m) => T.Text -> LoggerT m ()
info = log Info 

warn :: (Monad m) => T.Text -> LoggerT m ()
warn = log Warning

error :: (Monad m) => T.Text -> LoggerT m () 
error = log Error

fatal :: (Monad m) => T.Text -> LoggerT m () 
fatal = log Fatal
