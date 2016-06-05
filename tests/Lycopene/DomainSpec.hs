module Lycopene.DomainSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Lycopene.Core
import           Lycopene.Environment
import           Lycopene.Configuration
import qualified Lycopene.Core.Project as P
import qualified Lycopene.Core.Sprint as S

config :: Configuration
config = defaultConfiguration

context' :: IO Context
context' = connect config >>= \c ->
            return $ Context 0 (mkDataSource c)

runL :: Lycopene a -> IO (Either LycoError a)
runL l = context' >>= runLycopene (createDatabase >> l)

domainSuccess :: (Eq a, Show a) => Lycopene a -> a -> Expectation
x `domainSuccess` e = runL x `shouldReturn` Right e

spec :: Spec
spec = describe "Domain" $ do
    describe "Project" $ do
      it "can be inserted as an inbox project" $ do
        let x = P.inbox >> P.allProjects
        x `domainSuccess` [P.Project 0 P.inboxProjectName (Just P.inboxProjectDesc)]

      it "cen be inserted as a new project" $ do
        let x = P.ProjectV { P.vName = "test" , P.vDescription = Just "descr" }
        (P.newProject x >> fmap (P.name . head) (P.projectByName "test")) `domainSuccess` "test" 

    describe "Sprint" $ do
      it "create new project and sprint" $ do
        let x = S.newProjectAndSprint "project" (Just "project desc")
        x `domainSuccess` (1,1)

      it "can be created as a new sprint" $ do
        let x = S.SprintV
              { S.vName = "new sprint"
              , S.vDescription = Just "new description"
              , S.vProjectId = 0 -- global inbox
              , S.vStartOn = Nothing
              , S.vEndOn = Nothing}
        (S.newSprint x >> fmap (S.name . head) (S.sprintByProjectAndName 0 "new sprint")) `domainSuccess` "new sprint"
