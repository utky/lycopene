module Lycopene.DomainSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Lycopene.Core
import           Lycopene.Environment
import           Lycopene.Configuration
import qualified Lycopene.Core.Project as P
import qualified Lycopene.Core.Sprint as S

config :: Configuration
config = Configuration "bla" ":memory:" 0

context' :: IO Context
context' = connect config >>= \c ->
            return $ Context 0 (mkDataSource c)

runL :: Lycopene a -> IO (Either LycoError a)
runL l = context' >>= runLycopene (createDatabase >> l)

spec :: Spec
spec = do
  describe "Domain" $ do
    describe "Project" $ do
      it "insert an inbox project" $ do
        let x = P.inbox >> P.allProjects
        runL x `shouldReturn` Right [P.Project 0 P.inboxProjectName (Just P.inboxProjectDesc)]

    describe "Sprint" $ do
      it "create new project and sprint" $ do
        let x = S.newProjectAndSprint "project" (Just "project desc")
        runL x `shouldReturn` Right (1,1)
