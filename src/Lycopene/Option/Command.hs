module Lycopene.Option.Command
    ( LycoCommand (..)
    , Command(..)
    , AdminCmd(..)
    , OperCmd(..)
    ) where


import           Lycopene.Option.Common (CommonOption(..))

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

data OperCmd = Init (Maybe String) (Maybe String) FilePath
             | New String (Maybe String)
             | Mod (Maybe String) (Maybe String)
             | Ls Bool
             | Pj
             | Sp
             | Run (Maybe Integer) (Maybe Int)
             deriving (Show)
             

-------------------------------------------------------------------------------

data LycoCommand = LycoCommand CommonOption Command deriving (Show)


-- runLycoCommand :: LycoCommand -> IO LycoResult
-- runLycoCommand (LycoCommand c action) = config >>= (runWithConfiguration action) where
--   runWithConfiguration = runLycopeneT . runAction
--   config = fmap configure $ (commonOptsWithHome c) <$> getHomeDirectory

