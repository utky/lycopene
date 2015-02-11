{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Issue.Entity where

import           Lycopene.Core.Database (defineTable)

$(defineTable "issue")
