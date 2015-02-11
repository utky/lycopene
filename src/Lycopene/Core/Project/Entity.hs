{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Project.Entity where

import           Lycopene.Core.Database (defineTable)

$(defineTable "project")

