{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.LabeledIssue where

import           Database.Relational.Query
import           Lycopene.Database.Relational.TH (defineRelationFromDB)

$(defineRelationFromDB "labeled_issue")

