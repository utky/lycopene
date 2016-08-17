{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.Record where

import           Data.Time (UTCTime)
import           Database.HDBC.Query.TH (makeRecordPersistableDefault)
import           Database.Relational.Query
import           Lycopene.Database.Relational.TH (defineRelationFromDB)

$(defineRelationFromDB "record")

selectRecordByIssue :: Relation String Record
selectRecordByIssue = relation' . placeholder $ \ph -> do
  r <- query record
  wheres $ r ! issueId' .=. ph
  return r

data RecordV = RecordV
             { vIssueId :: String
             , vStartOn :: UTCTime
             , vEndOn :: Maybe UTCTime
             } 

$(makeRecordPersistableDefault ''RecordV)

piRecordV :: Pi Record RecordV
piRecordV = RecordV |$| issueId'
                    |*| startOn'
                    |*| endOn'

insertRecordV :: Insert RecordV
insertRecordV = typedInsert tableOfRecord piRecordV


