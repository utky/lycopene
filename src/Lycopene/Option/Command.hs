module Lycopene.Option.Command
    ( LycoCommand (..)
    , Command(..)
    ) where


import           Lycopene.Option.Common (CommonOption(..))

-------------------------------------------------------------------------------

-- | Command describes what to do for the application.
-- This simply indicates user input from command line.
data Command = 
             -- | version 
               Version
             -- | configure DIR
             | Configure
             | Start Int FilePath
             deriving (Show)

-------------------------------------------------------------------------------

data LycoCommand = LycoCommand CommonOption Command deriving (Show)


-- runLycoCommand :: LycoCommand -> IO LycoResult
-- runLycoCommand (LycoCommand c action) = config >>= (runWithConfiguration action) where
--   runWithConfiguration = runLycopeneT . runAction
--   config = fmap configure $ (commonOptsWithHome c) <$> getHomeDirectory

