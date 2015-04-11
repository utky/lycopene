module Lycopene.Option.Command
    ( LycoCommand (..)
    , LycoAction (..)
    , Command(..)
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe)
import           Options.Applicative
import           System.Directory
import           System.Environment (lookupEnv)
import qualified Data.Text as T

import           Lycopene.Core
import           Lycopene.Option.Common
import           Lycopene.Configuration
import           Lycopene.Process


-------------------------------------------------------------------------------

-- | Command describes what to do for the application.
-- This simply indicates user input from command line.
data Command =
             -- | version 
             Version
             -- | configure DIR
             | Configure FilePath
             -- | init DIR
             | Init FilePath


-- | Transform a description of command to runnable process
runCommand :: Command -> ProcessM IO
runCommand Version = undefined
runCommand (Configure dest) = undefined
runCommand (Init dest) = undefined

-------------------------------------------------------------------------------

data LycoCommand = LycoCommand CommonOption Command

type LycoAction = ProcessM IO

-- runLycoCommand :: LycoCommand -> IO LycoResult
-- runLycoCommand (LycoCommand c action) = config >>= (runWithConfiguration action) where
--   runWithConfiguration = runLycopeneT . runAction
--   config = fmap configure $ (commonOptsWithHome c) <$> getHomeDirectory

