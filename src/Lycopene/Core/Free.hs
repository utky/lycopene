module Lycopene.Core.Free where

data Free f a = Free (f (Free f a)) | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free x) = Free $ fmap (fmap f) x

instance Functor f => Applicative (Free f) where
  pure = Pure
  (Pure f) <*> x = fmap f x
  (Free f) <*> x = Free $ fmap (<*> x) f

instance Functor f => Monad (Free f) where
  return = pure
  (Pure x) >>= k = k x
  (Free x) >>= k = Free $ fmap (>>= k) x

foldF :: Functor f => (f a -> a) -> Free f a -> a
foldF phi (Free x) = phi $ fmap (foldF phi) x
foldF _ (Pure x) = x

liftF :: Functor f => f a -> Free f a
liftF x = Free $ fmap Pure x
