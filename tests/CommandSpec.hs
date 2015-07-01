module CommandSpec (spec) where

import           Test.Hspec
import           System.Directory (getCurrentDirectory, createDirectoryIfMissing, removeDirectory, removeDirectoryRecursive, removeFile, doesFileExist)
import           System.FilePath ((</>))
import           Lycopene.Core
import           Lycopene.Option
import           Lycopene.Process
import           Lycopene.Configuration
import           Control.Exception (bracket)
import           Control.Applicative ((<|>))
{-
runCommand :: Configuration -> LycoCommand -> IO Result
runCommand cfg cmd = runProcess' $ processCommand cfg cmd

specHome :: FilePath -> FilePath
specHome = (</>".lyco-spec")
-}

config :: Configuration
config = Configuration "dummy" ":memory:" 0

commonOpt :: CommonOption
commonOpt = CommonOption False (lycoHome config) False

spec :: Spec
spec = do
  describe "version" $ do
    it "succeeds" $ do 
      (runPure $ buildProcess (LycoCommand commonOpt Version) config) `shouldReturn` []
      pendingWith "not implemented"

  describe "configure" $ do
    it "generate the schema" $ do 
      -- runCommand c (LycoCommand (cfg2co c) (Administration Configure)) `shouldReturn` Success
      pendingWith "not implemented"

  describe "init" $ do
    it "create a local config" $ do 
      pendingWith "not implemented"
    it "create a new project" $ do 
      -- runCommand c (LycoCommand (cfg2co c) (Operation (Init Nothing Nothing "."))) `shouldReturn` Success
      pendingWith "not implemented"
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

