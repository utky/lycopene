{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Sprint where

import           Lycopene.Core.Database (defineTable)

$(defineTable "sprint")
