{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Issue
    ( Issue(..)
    ) where

import           Lycopene.Core.DataSource (defineTable)

$(defineTable "issue")
