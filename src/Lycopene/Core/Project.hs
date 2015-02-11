{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Project where

import           Lycopene.Core.Database.DataSource (defineTable)

$(defineTable "project")

