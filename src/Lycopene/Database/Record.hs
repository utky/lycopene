{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Record where

import           Data.Time.LocalTime
import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           Lycopene.Database (defineTable)

$(defineTable "record")

selectRecordByIssue :: Relation Integer Record
selectRecordByIssue = relation' . placeholder $ \ph -> do
  r <- query record
  wheres $ r ! issueId' .=. ph
  return r

data RecordV = RecordV
             { vIssueId :: Integer
             , vStartOn :: LocalTime
             , vEndOn :: Maybe LocalTime
             } 

$(makeRecordPersistableDefault ''RecordV)

piRecordV :: Pi Record RecordV
piRecordV = RecordV |$| issueId'
                    |*| startOn'
                    |*| endOn'

insertRecordV :: Insert RecordV
insertRecordV = typedInsert tableOfRecord piRecordV


