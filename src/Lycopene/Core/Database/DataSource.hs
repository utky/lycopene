{-# LANGUAGE TemplateHaskell #-}
module Lycopene.Core.Database.DataSource
      ( connect
      , defineTable
      , createTables
      ) where

import           Data.Time (Day, LocalTime)
import           Database.HDBC (runRaw)
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.HDBC.Schema.Driver (typeMap)
import           Database.HDBC.Schema.SQLite3 (driverSQLite3)
import           Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import           Database.Record.TH (derivingShow)
import           Language.Haskell.TH (Q, Dec, TypeQ)

import           Lycopene.Core.Database.Datapath (mkDatapath, tempDatapath)
import           Lycopene.Core.Database.Schema (schema)
import           Lycopene.Configuration

-- | Connect database with specified configuration
connect :: Configuration -> IO Connection
connect = connectSqlite3 . mkDatapath . lycoHome

convTypes :: [(String, TypeQ)]
convTypes =
        [ ("float", [t|Double|])
        , ("date", [t|Day|])
        , ("datetime", [t|LocalTime|])
        , ("double", [t|Double|])
        , ("varchar", [t|String|])
        , ("integer", [t|Integer|])
        ]

defineTable :: String -> Q [Dec]
defineTable tableName =
  defineTableFromDB
    connWithSchema
    (driverSQLite3 { typeMap = convTypes })
    "main"
    tableName
    [derivingShow]
  where
    conn = connectSqlite3 tempDatapath
    connWithSchema = conn >>= (\c -> createTables c >> (return c))

createTables :: Connection -> IO ()
createTables c = runRaw c schema
