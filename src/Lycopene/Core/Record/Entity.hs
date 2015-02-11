{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Record.Entity where

import           Lycopene.Core.Database (defineTable)

$(defineTable "record")

