module Lycopene.Core.Pure
    ( VStore
    , VResult
    , runVStore
    , initial
    , values
    , save
    , remove
    , fetch
    , catch
    , notFound
    ) where

import           Prelude hiding (lookup)
import           Control.Monad.Trans (lift)
import           Control.Monad.Except (ExceptT, throwError, runExceptT)
import           Control.Monad.State.Strict (State, evalState, get, modify)
import           Data.Map.Strict (Map, empty, lookup, insert, delete)

data VStoreException = NoSuchElement

type VResult = Either VStoreException

-- | Volatile store
type VStore k v a = ExceptT VStoreException (State (Map k v)) a

runVStore :: Map k v -> VStore k v a -> Either VStoreException a
runVStore s = (\m -> evalState m s) . runExceptT

initial :: Map k v
initial = empty

values :: VStore k v [v]
values = foldr (:) [] <$> (lift get)

save :: Ord k => k -> v -> VStore k v ()
save k v = lift $ modify (insert k v)

remove :: Ord k => k -> VStore k v ()
remove k = lift $ modify (delete  k)

fetch :: Ord k => k -> VStore k v v
fetch k = fmap (lookup k) (lift get) >>= catch

catch :: Maybe a -> VStore k v a
catch Nothing = throwError NoSuchElement
catch (Just x) = return x

notFound :: VResult a -> Bool
notFound (Left NoSuchElement) = True
notFound _ = False
