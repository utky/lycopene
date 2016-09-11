{-# LANGUAGE TemplateHaskell #-}
module Lycopene.Database.Relational.TH where

import           Lycopene.Database.DataSource (connect)
import           Lycopene.Database.Relational.Schema (schema)
import           Data.Time (UTCTime, Day, LocalTime)
import           Database.HDBC (runRaw)
import           Database.HDBC.Schema.Driver (typeMap)
import           Database.HDBC.Schema.SQLite3 (driverSQLite3)
import           Database.Relational.Query (defaultConfig)
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.Record.TH (derivingShow)
import           Language.Haskell.TH (Q, Dec, TypeQ)

defineRelationFromDB
  :: String	-- ^ Table name
  -> Q [Dec]
defineRelationFromDB tableName =
  defineTableFromDB
    connWithSchema
    (driverSQLite3 { typeMap = convTypes })
    "main"
    tableName
    [''Show]
  where
    conn = connect ":memory:"
    connWithSchema = conn >>= (\c -> runRaw c schema >> (return c))

type Column = (String, TypeQ)

convTypes :: [Column]
convTypes =
        [ ("float", [t|Double|])
        , ("date", [t|Day|])
        , ("datetime", [t|UTCTime|])
        , ("timestamp", [t|UTCTime|])
        , ("double", [t|Double|])
        , ("varchar", [t|String|])
        , ("integer", [t|Int|])
        ]

date :: String -> Column
date n = (n, [t|Day|])

datetime :: String -> Column
datetime n = (n, [t|LocalTime|])

varchar :: String -> Column
varchar n = (n, [t|String|])

integer :: String -> Column
integer n = (n, [t|Int|])

double :: String -> Column
double n =  (n, [t|Double|])

float :: String -> Column
float n =  (n, [t|Float|])
