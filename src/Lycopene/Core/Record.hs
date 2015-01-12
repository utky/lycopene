{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Record
    ( Record(..)
    ) where

import           Lycopene.Core.DataSource (defineTable)

$(defineTable "record")
