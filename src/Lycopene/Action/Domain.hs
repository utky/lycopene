module Lycopene.Action.Domain where


import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Control.Monad.Reader (ReaderT(..))

import           Lycopene.Core
import           Lycopene.Configuration


type Domain a = ReaderT Context IO (Either LycoError a)


-- runDomain :: Domain a -> Context -> IO (Either LycoError a)
-- runDomain dom ctx = do
--   let ds = dataSource ctx
--       connector ctx' conn = ctx' { dataSource = (mkDataSource . ConnWrapper) conn } 
--   withTransaction c (\_ -> runReaderT dom ctx)

mapDomain :: Lycopene a -> Domain a
mapDomain l = ReaderT $ runLycopene l

connectDataSource :: Configuration -> Resource DataSource
connectDataSource = (fmap mkDataSource) . connection . connectSqlite3 . datapath

fetchResource :: Configuration -> Resource Context
fetchResource cfg = Context
                    <$> return 0 -- FIXME
                    <*> connectDataSource cfg

