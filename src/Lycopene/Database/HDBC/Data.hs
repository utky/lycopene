module Lycopene.Database.HDBC.Data where

import           Database.HDBC (SqlValue)
import           Lycopene.Core (Project(..))

class Entity a where
  decode :: [SqlValue] -> Maybe a

instance Entity Project where
  decode :: [SqlValue] -> Maybe a

toProject :: [SqlValue] -> Maybe Project
toProject = undefined

-- |
-- 'SELECT a, b, c FROM table WHERE x = ? AND y = ?'
-- Select (x, y) (a, b, c)

