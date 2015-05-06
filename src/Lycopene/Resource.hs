module Lycopene.Resource where

import           Lycopene.Option (LycoCommand(..), CommonOption(..), Command(..), commonOptsWithHome)
import           Lycopene.Configuration (Configuration(..))
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.Trans (lift, liftIO)
import           Control.Monad.Trans.Error (ErrorT(..), Error(..))
import           System.FilePath
import           System.Directory (doesFileExist, doesDirectoryExist, getCurrentDirectory, getHomeDirectory)
import           Data.Char (isDigit)


configResource :: LycoCommand -> IO Configuration
configResource (LycoCommand co _) = 
  let abshome = expandHome <$> home <*> ( return . homeLocation $ co)
      lycoH = abshome
      dpath = (</> "issues.db") <$> abshome
      localcfg  = (</> ".lyco.conf") <$> here
  -- FIXME: use local config rather than default project
  in Configuration <$> lycoH <*> dpath <*> (return defaultTargetProject)


-- | load Configuration from filesystem
loadConfig :: FilePath -> String -> IO (Maybe Configuration)
loadConfig home dp = do
  homeExists        <- doesDirectoryExist home
  datapathExists    <- doesFileExist dp
  localConfig       <- combine <$> getCurrentDirectory <*> (return ".lyco.config")
  localConfigExists <- doesFileExist localConfig
  if homeExists && datapathExists
    then if localConfigExists
      then (Just . (Configuration home dp)) `fmap` readTargetProject localConfig
      else return (Just $ Configuration home dp defaultTargetProject)
    -- FIXME: print debug
    else putStrLn "home or dp not exist" >> return Nothing
  
defaultTargetProject :: Integer
defaultTargetProject = 0

readTargetProject :: FilePath -> IO Integer
readTargetProject f = 
  let isDigitStr = foldr (\a b -> isDigit a && b) True
      configlines = (lines) `fmap` readFile f 
      readProjectId (x:_) = if isDigitStr x then (read x :: Integer) else defaultTargetProject
      readProjectId [] = defaultTargetProject
  in readProjectId `fmap` configlines


here :: IO FilePath
here = getCurrentDirectory

home :: IO FilePath
home =  getHomeDirectory

aquire :: Configuration -> IO (Either String Configuration)
aquire cfg = do
  let home = lycoHome cfg
      dp = datapath cfg
  homeExists        <- doesDirectoryExist home
  datapathExists    <- doesFileExist dp
  localConfig       <- combine <$> here <*> (return ".lyco.config")
  localConfigExists <- doesFileExist localConfig
  if homeExists && datapathExists
    then if localConfigExists
      then (Right . (Configuration home dp)) `fmap` readTargetProject localConfig
      else return (Right $ Configuration home dp defaultTargetProject)
    else return . Left $ "Lyco Home: " ++ home ++ " exists?: " ++ (show homeExists)
      ++ ". Datapath: " ++ dp ++ " exists?: " ++ (show datapathExists)

expandHome :: FilePath -> FilePath -> FilePath
expandHome home ('~':xs) = home ++ xs
expandHome home path     = path

expandCurrent :: FilePath -> FilePath -> FilePath
expandCurrent current ('.':xs) = current ++ xs
expandCurrent current path     = path

