{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Project.Entity where

import           Database.Relational.Query
import           Lycopene.Core.Database (defineTable)

$(defineTable "project")

-- ここはもう少しhigher orderにできる entity -> tuple 
-- piTransient :: (entity -> tuple) -> Pi entity tuple
-- insertTransient :: Table entity -> Pi entity tuple -> Insert tuple

piTransient :: Pi Project (String, (Maybe String))
piTransient = name' >< description'

insertTransient :: Insert (String, Maybe String)
insertTransient = typedInsert tableOfProject piTransient
