module Lycopene.ApplicationSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Lycopene.SpecTool
import qualified Lycopene.Core as Core
import           Debug.Trace (trace)

spec :: Spec
spec = do
    before localEngine $ do
      describe "Event" $ do

        it "create a project" $ \engine -> do
          created@(Right (Core.Project pid _ _ _)) <- runEngine engine (Core.NewProject "new" Nothing)
          fetched <- runEngine engine (Core.FetchProject pid)
          fetched `shouldBe` created

        it "remove a project" $ \engine -> do
          (Right (Core.Project pid _ _ _)) <- runEngine engine (Core.NewProject "new" Nothing)
          runEngine engine (Core.RemoveProject pid)
          fetched <- runEngine engine Core.AllProject
          (mapR length fetched) `shouldBe` (Right 0) 

        it "create multiple projects" $ \engine -> do
          one <- runEngine engine (Core.NewProject "new1" Nothing)
          two <- runEngine engine (Core.NewProject "new2" Nothing)
          fetched <- runEngine engine Core.AllProject
          (mapR length fetched) `shouldBe` (Right 2) 

        it "fetch backlog sprints created on project creation" $ \engine -> do
          (Right (Core.Project pid _ _ _)) <- runEngine engine (Core.NewProject "new" Nothing)
          fetched <- runEngine engine (Core.FetchProjectSprint pid)
          (mapR length fetched) `shouldBe` (Right 1)

        it "fetch a backlog sprint" $ \engine -> do
          (Right (Core.Project pid _ _ _)) <- runEngine engine (Core.NewProject "new" Nothing)
          (Right (fetched:ss)) <- runEngine engine (Core.FetchProjectSprint pid)
          (Core.sprintName fetched) `shouldBe` "backlog"

        it "create a issue" $ \engine -> do
          (Right (Core.Project pid _ _ _)) <- runEngine engine (Core.NewProject "new" Nothing)
          created@(Right (Core.Issue isId _ _ _)) <- runEngine engine (Core.NewIssue "issueName" Nothing pid Nothing)
          fetched <- runEngine engine (Core.FetchIssue isId)
          fetched `shouldBe` created

        it "remove a issue" $ \engine -> do
          (Right (Core.Project pid _ _ _)) <- runEngine engine (Core.NewProject "new" Nothing)
          (Right (Core.Issue isId _ _ _)) <- runEngine engine (Core.NewIssue "issueName" Nothing pid Nothing)
          runEngine engine (Core.RemoveIssue isId)
          fetched <- runEngine engine (Core.FetchIssues pid Nothing Core.IssueOpen)
          (mapR length fetched) `shouldBe` (Right 0)

