{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.Pomodoro where

import           Data.Time (UTCTime)
import           Database.Relational.Query
import           Lycopene.Database.Relational.TH (defineRelationFromDB)

$(defineRelationFromDB "pomodoro")

selectRecordByIssue :: Relation String Pomodoro
selectRecordByIssue = relation' . placeholder $ \ph -> do
  r <- query pomodoro
  wheres $ r ! issueId' .=. ph
  return r

-- data PomodoroV = PomodoroV
--              { vIssueId :: String
--              , vStartOn :: UTCTime
--              , vEndOn :: Maybe UTCTime
--              } 
-- 
-- $(makePomodoroPersistableDefault ''PomodoroV)
-- 
-- piPomodoroV :: Pi Pomodoro PomodoroV
-- piPomodoroV = PomodoroV |$| issueId'
--                         |*| startOn'
--                         |*| endOn'
-- 
-- insertPomodoroV :: Insert PomodoroV
-- insertPomodoroV = typedInsert tableOfPomodoro piPomodoroV
-- 

