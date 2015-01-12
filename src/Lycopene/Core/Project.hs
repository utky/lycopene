{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Project
    ( Project(..)
    ) where

import           Lycopene.Core.DataSource (defineTable)

$(defineTable "project")
