{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Sprint.Entity where

import           Lycopene.Core.Database (defineTable)

$(defineTable "sprint")

