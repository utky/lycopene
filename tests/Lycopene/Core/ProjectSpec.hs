module Lycopene.Core.ProjectSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Lycopene.Core

runResult :: VResult Bool -> Bool
runResult (Right b) = b
runResult _ = False

prop_fetchIdempotent :: Bool
prop_fetchIdempotent =
  let first = fetchAllProject
      second = fetchAllProject
  in  runResult $ runProjectPure $ (==) <$> first <*> second

prop_addIdempotent :: Name -> Description -> Bool
prop_addIdempotent n d = 
  let added = addProject (newProject n d)
  in  runResult $ runProjectPure $ (==) <$> added <*> added
 
prop_statusRefl :: Name -> Description -> Bool
prop_statusRefl n d =
  let new = newProject n d
      iso = activateProject . deactivateProject
      modified = (iso (newProject n d))
  in  runResult $ runProjectPure $ (==) <$> new <*> modified 

prop_addFetch :: Name -> Description -> Bool
prop_addFetch n d =
  let new = newProject n d
      added = addProject new
      fetched = added >>= (fetchByIdProject . projectId)
  in runResult $ runProjectPure $ (==) <$> (return n) <*> (fmap projectName fetched)

prop_removeFetch :: Name -> Description -> Bool
prop_removeFetch n d =
  let new = newProject n d
      removed = removeProject (addProject new)
      fetched = removed >>= (fetchByNameProject . projectName)
  in notFound $ runProjectPure fetched

spec :: Spec
spec = do
  describe "Project" $ do
    context "primitives" $ do
      it "fetch project idempotently" $ do
        property prop_fetchIdempotent
      it "preserves identitfy of added project" $ do
        property prop_addIdempotent
      it "has reflexivity with activate and deactivate" $ do
        property prop_statusRefl
      it "can fetch an added project" $ do
        property prop_addFetch
      it "cannot fetch a removed project" $ do
        property prop_removeFetch

