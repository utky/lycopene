{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Label where

import           Database.Relational.Query
import           Lycopene.Database (defineTable)

$(defineTable "label")
