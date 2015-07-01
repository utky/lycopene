module Lycopene.FreeSpec (spec) where


import           Test.Hspec
import           Test.QuickCheck
import           Control.Monad.Identity
import           Lycopene.Core.Free

equalsF :: (Eq a, Functor f) => (f Bool -> Bool) -> Free f a -> Free f a -> Bool
equalsF _ (Pure x) (Pure y) = x == y
equalsF phi (Free x) y = phi $ fmap (equalsF phi y) x

liftPure :: a -> Free Identity a
liftPure = Free . return . Pure

-- | fmap id x = x
prop_functor_identity_law :: Integer -> Bool
prop_functor_identity_law x = let m = liftPure x
                              in  equalsF runIdentity (fmap id m) m

-- | fmap f . fmap g = fmap (f . g)
prop_functor_compose_law :: Integer -> Bool
prop_functor_compose_law x = let f = (+1)
                                 g = (*2)
                                 m = Pure x
                             in equalsF runIdentity ((fmap f . fmap g) m) ((fmap (f . g)) m)

-- | pure id <*> v = v
prop_applicative_identity_law :: Integer -> Bool
prop_applicative_identity_law x = equalsF runIdentity (pure id <*> (liftPure x)) (liftPure x)

-- | pure f <*> pure x = pure (f x)
prop_applicative_homomorphism_law :: Integer -> Bool
prop_applicative_homomorphism_law x = 
    equalsF runIdentity (pure id <*> pure x) (pure $ id x)

-- | u <*> pure y = pure ($ y) <*> u
prop_applicative_interchange_law :: Integer -> Bool
prop_applicative_interchange_law x =
    equalsF runIdentity (pure id <*> pure x) (pure ($ x) <*> pure id)

prop_identity_for_foldF :: Int -> Bool
prop_identity_for_foldF x = x == foldF runIdentity (liftPure x)

spec :: Spec
spec = do
  describe "Free" $ do
    it "satisfy functor identity law" $ property prop_functor_identity_law
    it "satisfy functor compose law" $ property prop_functor_compose_law
    it "satisfy applicative identity law" $ property prop_applicative_identity_law
    it "satisfy applicative homomorphism law" $ property prop_applicative_homomorphism_law
    it "satisfy applicative interchange law" $ property prop_applicative_interchange_law
    it "fold into source value" $ property prop_identity_for_foldF
