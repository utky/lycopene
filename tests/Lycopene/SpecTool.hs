module Lycopene.SpecTool 
  ( localEngine
  , mapR
  , module Lycopene.Application
  ) where

import           Lycopene.Application (AppEngine(..), appEngine, runEngine)
import           Lycopene.Database (connect, runDB, rawDB, DBException(..))
import           Lycopene.Database.Relational (schema)

mapR :: (b -> c) -> Either a b -> Either a c
mapR f (Right x) = Right (f x)
mapR _ (Left y) = Left y


localEngine :: IO AppEngine
localEngine = do
  ds <- connect ":memory:"
  _ <- runDB (rawDB schema) ds
  return $ appEngine ds
  -- case r of
  --   (Left e) -> return $ AppEngine $ \_ -> return (Left e)
  --   (Right _) -> return $ appEngine ds
