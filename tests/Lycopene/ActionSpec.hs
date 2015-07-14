module Lycopene.ActionSpec (spec) where


import           Test.Hspec
import           Test.QuickCheck
import           Lycopene.Action

spec :: Spec
spec = do
  describe "Action" $ do
    it "lift pure value" $ do
      pendingWith "to be implemented"

    it "lift file system operation" $ do
      pendingWith "to be implemented"

    it "lift domain operation" $ do
      pendingWith "to be implemented"

{-

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

-}

