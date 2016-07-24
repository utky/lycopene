{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Color where

import           Database.Relational.Query
import           Lycopene.Database (defineTable)

$(defineTable "color")


