{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.SprintIssue where

import           Database.Relational.Query
import           Lycopene.Database.Relational.TH (defineRelationFromDB)


$(defineRelationFromDB "sprint_issue")
