{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Core.Record.Entity where

import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           Lycopene.Core.Database (defineTable)

$(defineTable "record")

data RecordV = RecordV
             { vIssueId :: Integer
             , vStartOn :: Maybe Integer
             , vEndOn :: Maybe Integer
             } 

$(makeRecordPersistableDefault ''RecordV)

piRecordV :: Pi Record RecordV
piRecordV = RecordV |$| issueId'
                    |*| startOn'
                    |*| endOn'

insertRecordV :: Insert RecordV
insertRecordV = typedInsert tableOfRecord piRecordV
