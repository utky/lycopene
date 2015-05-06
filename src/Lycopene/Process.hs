{-# LANGUAGE RankNTypes #-}
module Lycopene.Process
    ( processCommand
    , module Lycopene.Process.Core
    , module Lycopene.Process.Version
    , module Lycopene.Process.Configure
    ) where

import qualified Data.Text as T
import           Pipes.Lift (runReaderP)
import           Control.Monad.Trans (lift, liftIO)
import           Control.Monad.Reader (ask)
import           Lycopene.Option (LycoCommand(..), CommonOption(..), Command(..), AdminCmd(..), OperCmd(..))
import           Lycopene.Process.Core (LycoError(..), Result(..), Chunk, ProcessR, ProcessM, runProcess, runProcess', out, debug, MonadIO, failure)
import           Lycopene.Process.Version (version)
import           Lycopene.Process.Configure (configure)
import           Lycopene.Process.Init (initialize)
import           Lycopene.Process.Ls (listIssues)
import           Lycopene.Process.New (newIssue)
import           Lycopene.Resource (aquire)
import           Lycopene.Configuration (Configuration(..))
import           Lycopene.Core (config)
import           System.FilePath ((</>))


processCommand :: (MonadIO m) => Configuration -> LycoCommand -> ProcessM m
processCommand cfg (LycoCommand cmn (Administration admin)) = runReaderP cfg (processAdmin cmn admin)
processCommand cfg (LycoCommand cmn (Operation oper)) = do
  ec <- liftIO $ aquire cfg
  case ec of
    Right c -> runReaderP c (processOper cmn oper)
    Left s -> debug (T.pack s) >> failure
  

processAdmin :: (MonadIO m) => CommonOption -> AdminCmd -> ProcessR m
processAdmin cmn Version = version
processAdmin cmn Configure = lift ask >>= (\cfg -> configure (datapath cfg))

processOper :: (MonadIO m) => CommonOption -> OperCmd -> ProcessR m
processOper cmn (Init mName mDesc path) = initialize mName mDesc path
processOper cmn (Ls all) = listIssues all
processOper cmn (New title mDesc) = newIssue title mDesc
processOper cmn (Run i) = undefined
