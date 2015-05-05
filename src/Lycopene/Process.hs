{-# LANGUAGE RankNTypes #-}
module Lycopene.Process
    ( processCommand
    , module Lycopene.Process.Core
    , module Lycopene.Process.Version
    , module Lycopene.Process.Configure
    ) where

import           Pipes.Lift (runReaderP)
import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (ask)
import           Lycopene.Option (LycoCommand(..), CommonOption(..), Command(..), AdminCmd(..), OperCmd(..))
import           Lycopene.Process.Core (LycoError(..), Result(..), Chunk, ProcessR, ProcessM, runProcess, runProcess', out, debug, MonadIO)
import           Lycopene.Process.Version (version)
import           Lycopene.Process.Configure (configure)
import           Lycopene.Configuration (Configuration(..))
import           Lycopene.Core (config)
import           System.FilePath ((</>))


processCommand :: (MonadIO m) => Configuration -> LycoCommand -> ProcessM m
processCommand cfg (LycoCommand cmn (Administration admin)) = runReaderP cfg (processAdmin cmn admin)
processCommand cfg (LycoCommand cmn (Operation oper)) = runReaderP cfg (processOper cmn oper)

processAdmin :: (MonadIO m) => CommonOption -> AdminCmd -> ProcessR m
processAdmin cmn Version = version
processAdmin cmn Configure = lift ask >>= (\cfg -> configure (datapath cfg))

processOper :: (MonadIO m) => CommonOption -> OperCmd -> ProcessR m
processOper cmn (Init fp) = undefined

