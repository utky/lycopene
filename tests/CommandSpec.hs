module CommandSpec (spec) where

import           Test.Hspec
import           System.Directory (getCurrentDirectory, createDirectoryIfMissing, removeDirectory, removeDirectoryRecursive, removeFile, doesFileExist)
import           System.FilePath ((</>))
import           Lycopene.Option
import           Lycopene.Process (runProcess', processCommand, Result(..))
import           Lycopene.Configuration
import           Control.Exception (bracket)
import           Control.Applicative ((<|>))

runCommand :: Configuration -> LycoCommand -> IO Result
runCommand cfg cmd = runProcess' $ processCommand cfg cmd

specHome :: FilePath -> FilePath
specHome = (</>".lyco-spec")

config :: FilePath -> Configuration
config home = Configuration
       { lycoHome = home
       , datapath = ":memory:"
       , targetProject = 0
       }

withConfig :: (Configuration -> IO a) -> IO a
withConfig = 
  let tmp = do
        cd <- getCurrentDirectory
        let home = specHome cd
        createDirectoryIfMissing True home
        return $ config home
      rmtmp cfg = do
        let home = lycoHome cfg
        removeDirectoryRecursive home
  in bracket tmp rmtmp

cfg2co :: Configuration -> CommonOption
cfg2co cfg = CommonOption False (lycoHome cfg) False


spec :: Spec
spec = do
  describe "version" $ do
    it "succeeds" $ withConfig $ \c -> do 
      runCommand c (LycoCommand (cfg2co c) (Administration Version)) `shouldReturn` Success

  describe "configure" $ do
    it "generate the schema" $ withConfig $ \c -> do 
      runCommand c (LycoCommand (cfg2co c) (Administration Configure)) `shouldReturn` Success

  describe "init" $ do
    it "create a local config" $ withConfig $ \c -> do 
      pendingWith "not implemented"
    it "create a new project" $ withConfig $ \c -> do 
      runCommand c (LycoCommand (cfg2co c) (Operation (Init Nothing Nothing "."))) `shouldReturn` Success
    it "create a backlog sprint" $ do 
      pendingWith "not implemented"

  describe "new" $ do
    it "create a new issue with title" $ do 
      pendingWith "not implemented"

  describe "mod" $ do
    it "modify the title of specified issue" $ do 
      pendingWith "not implemented"
    it "modify the project of specified issue" $ do 
      pendingWith "not implemented"
    it "modify the estimate of specified issue" $ do 
      pendingWith "not implemented"

  describe "rm" $ do
    it "remove the specified issue" $ do 
      pendingWith "not implemented"

  describe "done" $ do
    it "change its state to done" $ do 
      pendingWith "not implemented"

  describe "ls" $ do
    it "list issues of targeted project" $ do 
      pendingWith "not implemented"

  describe "run" $ do
    it "start timer process" $ do 
      pendingWith "not implemented"

