module Lycopene.Action.Domain where


import           System.Directory (doesFileExist)
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

readProjectId :: String -> Integer
readProjectId = read

readFileR :: FilePath -> Resource Integer
readFileR path = Resource $ do
  exists <- doesFileExist path
  if exists
    then (fmap readProjectId . readFile) path >>= (\c -> return (c, return()))
    else return (0, return())

fetchResource :: Configuration -> Resource Context
fetchResource cfg = Context
                    <$> (readFileR . projectConf) cfg
                    <*> connectDataSource cfg

