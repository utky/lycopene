module Lycopene.ApplicationSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Lycopene.SpecTool
import qualified Lycopene.Core as Core

spec :: Spec
spec = do
    before localEngine $ do
      describe "Event" $ do

        it "create a project" $ \engine -> do
          created <- runEngine engine (Core.NewProject "new" Nothing)
          fetched <- runEngine engine (Core.FetchProject "new")
          fetched `shouldBe` created

        it "remove a project" $ \engine -> do
          _ <- runEngine engine (Core.NewProject "new" Nothing)
          runEngine engine (Core.RemoveProject "new")
          fetched <- runEngine engine Core.AllProject
          (mapR length fetched) `shouldBe` (Right 0) 

        it "fetch backlog sprints created on project creation" $ \engine -> do
          _ <- runEngine engine (Core.NewProject "new" Nothing)
          fetched <- runEngine engine (Core.FetchProjectSprint "new")
          (mapR length fetched) `shouldBe` (Right 1)

        it "fetch a backlog sprint" $ \engine -> do
          _ <- runEngine engine (Core.NewProject "new" Nothing)
          fetched <- runEngine engine (Core.FetchSprint "new" "backlog")
          (mapR Core.sprintName fetched) `shouldBe` (Right "backlog")

        it "create a issue" $ \engine -> do
          _ <- runEngine engine (Core.NewProject "new" Nothing)
          created <- runEngine engine (Core.NewIssue "new" "backlog" "issue")
          fetched <- runEngine engine (Core.FetchIssues "new" "backlog" Core.IssueOpen)
          (mapR (Core.issueTitle . head) fetched) `shouldBe` (Right "issue")


