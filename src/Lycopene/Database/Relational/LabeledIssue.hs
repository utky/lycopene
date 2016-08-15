{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.LabeledIssue where

import           Database.Relational.Query
import           Lycopene.Database (defineTable)

$(defineTable "labeled_issue")

