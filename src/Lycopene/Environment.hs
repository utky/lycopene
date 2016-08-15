module Lycopene.Environment where

import           System.Directory
                   ( createDirectoryIfMissing
                   -- , XdgDirectory(..)
                   -- , getXdgDirectory
                   )

import           Lycopene.Directory

appName :: String
appName = "lycopene"

configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig appName

configPath :: IO FilePath
configPath = fmap (</> "config.yaml") configDir

dataDir :: IO FilePath
dataDir = getXdgDirectory XdgData appName

dataPath :: IO FilePath
dataPath = fmap (</> "issues.db") dataDir

initializeDirs :: IO ()
initializeDirs = do
  confDir <- configDir
  createDirectoryIfMissing True confDir
  dataDir <- dataDir
  createDirectoryIfMissing True dataDir
