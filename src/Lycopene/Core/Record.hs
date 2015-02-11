{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Record where

import           Lycopene.Core.Database.DataSource (defineTable)

$(defineTable "record")
