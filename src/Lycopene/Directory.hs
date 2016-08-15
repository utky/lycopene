module Lycopene.Directory
  ( XdgDirectory(..)
  , getXdgDirectory
  , module System.FilePath
  ) where

import           System.Directory (getHomeDirectory)
import           System.FilePath (normalise, isRelative, (</>))
import           System.IO.Error
                   ( ioeSetLocation
                   , modifyIOError
                   )
import           System.Environment (lookupEnv)

data XdgDirectory
  = XdgData
  | XdgConfig
  | XdgCache
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

getXdgDirectory :: XdgDirectory -> FilePath -> IO FilePath
getXdgDirectory xdgDir suffix =
  modifyIOError (`ioeSetLocation` "getXdgDirectory") $
  normalise . (</> suffix) <$>
  case xdgDir of
    XdgData   -> get False "XDG_DATA_HOME"   ".local/share"
    XdgConfig -> get False "XDG_CONFIG_HOME" ".config"
    XdgCache  -> get True  "XDG_CACHE_HOME"  ".cache"
  where
    get _ name fallback = do
      env <- lookupEnv name
      case env of
        Nothing                     -> fallback'
        Just path | isRelative path -> fallback'
                  | otherwise       -> return path
      where fallback' = (</> fallback) <$> getHomeDirectory

