module Lycopene.Core.Record.Service where

import           Control.Monad.Trans (liftIO)
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone, LocalTime)
import           Lycopene.Core.Monad
import           Lycopene.Core.Database
import qualified Lycopene.Core.Record.Entity as E

newRecord :: Integer -> LocalTime -> Lycopene Integer
newRecord i st = do
  et <- liftIO $ utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
  let v = E.RecordV { E.vIssueId = i , E.vStartOn = st , E.vEndOn = Just et }
  runPersist $ insertP E.insertRecordV v

history :: Integer -> Lycopene [E.Record]
history i = runPersist $ relationP E.selectRecordByIssue i
