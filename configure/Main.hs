module Main where

import           System.FilePath ((</>))
import           System.Directory (getHomeDirectory)
import           Lycopene.Schema (createTables)
import           Database.HDBC.Sqlite3 (connectSqlite3)

main :: IO ()
main = getHomeDirectory >>= connect >>= createTables where
  connect = connectSqlite3 . datapath . basedir
  basedir p = p </> ".lyco"
  datapath p = p </> "issues.db"

