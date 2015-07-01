module Lycopene.Action.Domain where


import           Database.HDBC (ConnWrapper(..), withTransaction)
import           Database.HDBC.Sqlite3 (connectSqlite3)

import           Lycopene.Core
import           Lycopene.Configuration
import           Control.Monad.Reader (ReaderT(..))

type Domain a = ReaderT Context IO (Either LycoError a)

runDomain :: Domain a -> Resource Context -> IO (Either LycoError a)
runDomain dom rs = runResource rs $ \ctx -> do
  let ds = dataSource ctx
  withDataSource ds $ \c ->
    withTransaction c (\_ -> runReaderT dom ctx)

mapDomain :: Lycopene a -> Domain a
mapDomain l = ReaderT $ runLycopene l


fetchResource :: Configuration -> Resource Context
fetchResource cfg = let conn = connection . connectSqlite3 $ datapath cfg
                    in Context
                       <$> return 0 -- FIXME
                       <*> fmap (mkDataSource . ConnWrapper) conn


