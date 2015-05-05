module Lycopene.Option.Command
    ( LycoCommand (..)
    , Command(..)
    , AdminCmd(..)
    , OperCmd(..)
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

-------------------------------------------------------------------------------

-- | Command describes what to do for the application.
-- This simply indicates user input from command line.
data Command = Administration AdminCmd | Operation OperCmd deriving (Show)

data AdminCmd = 
             -- | version 
             Version
             -- | configure DIR
             | Configure
             deriving (Show)

data OperCmd = Init FilePath
             | View
             | Add
             deriving (Show)
             

-------------------------------------------------------------------------------

data LycoCommand = LycoCommand CommonOption Command deriving (Show)


-- runLycoCommand :: LycoCommand -> IO LycoResult
-- runLycoCommand (LycoCommand c action) = config >>= (runWithConfiguration action) where
--   runWithConfiguration = runLycopeneT . runAction
--   config = fmap configure $ (commonOptsWithHome c) <$> getHomeDirectory

