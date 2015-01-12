{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Sprint
    ( Sprint(..)
    ) where

import           Lycopene.Core.DataSource (defineTable)

$(defineTable "sprint")
