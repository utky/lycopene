{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Issue where

import           Lycopene.Core.Database.DataSource (defineTable)

$(defineTable "issue")
