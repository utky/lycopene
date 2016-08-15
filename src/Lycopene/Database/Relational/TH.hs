{-# LANGUAGE TemplateHaskell #-}
module Lycopene.Database.TH where

import           Data.Time (Day, LocalTime)
import           Database.HDBC.Schema.Driver (typeMap)
import           Database.Relational.Query (defaultConfig)
import           Database.Relational.Query.TH (defineTable)
import           Database.Record.TH (derivingShow)
import           Language.Haskell.TH (Q, Dec, TypeQ)

defineRelation
  :: String	-- ^ Table name
  -> [(String, TypeQ)] -- ^ List of column name and type
  -> [Int] -- ^ Primary key index
  -> Maybe Int -- ^ Not null key index
  -> Q [Dec]
defineRelation tableName columns prim notnull =
  defineTable
    defaultConfig
    "main"
    tableName
    columns
    [derivingShow]
    prim
    notnull

type Column = (String, TypeQ)

convTypes :: [Column]
convTypes =
        [ ("float", [t|Double|])
        , ("date", [t|Day|])
        , ("datetime", [t|LocalTime|])
        , ("timestamp", [t|LocalTime|])
        , ("double", [t|Double|])
        , ("varchar", [t|String|])
        , ("integer", [t|Integer|])
        ]

date :: String -> Column
date n = (n, [t|Day|])

datetime :: String -> Column
datetime n = (n, [t|LocalTime|])

varchar :: String -> Column
varchar n = (n, [t|String|])

integer :: String -> Column
integer n = (n, [t|Integer|])

double :: String -> Column
double n =  (n, [t|Double|])

float :: String -> Column
float n =  (n, [t|Float|])
