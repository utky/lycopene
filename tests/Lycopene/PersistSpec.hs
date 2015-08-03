module Lycopene.PersistSpec (spec) where

import Test.Hspec

import           Lycopene.Core.Database
import qualified Lycopene.Core.Project as Project
import           Lycopene.Configuration

config = defaultConfiguration

runP :: Persist a -> IO a
runP p = connect config >>= unPersist p

runP' :: Persist a -> IO a
runP' x = runP (direct createTables >> x)

spec :: Spec
spec = do
  describe "Persist" $ do
    let projectA = Project.Project 1 "project" (Just "description")
        insert1 = insertP Project.insertProject projectA

    it "should create database on SQLite" $ do
      let defineP = direct createTables
      runP defineP `shouldReturn` ()

    it "should insert bulk Project" $ do
      runP' insert1 `shouldReturn` 1

    it "should query Project" $ do
      let projects = relationP Project.project ()
      runP' ((Project.name . head) `fmap` (insert1 >> projects)) `shouldReturn` "project"
      
