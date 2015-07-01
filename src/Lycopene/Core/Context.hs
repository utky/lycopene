module Lycopene.Core.Context where

import           Lycopene.Core.Database.DataSource (DataSource)

data Context = Context
             { targetProject :: Integer
             , dataSource :: DataSource
             }
