{-# LANGUAGE GADTs #-}
module Lycopene.Core.Pomodoro where

import           Lycopene.Core.Scalar
import           Lycopene.Core.Issue (IssueId)

type PomodoroId = Identifier

data Pomodoro
    = Pomodoro
    { pomodoroId :: !Identifier
    , pomodoroStartOn :: !DateTime
    , pomodoroEndOn :: !(Maybe DateTime)
    } deriving (Show)

instance Eq Pomodoro where
    x == y = (pomodoroId x) == (pomodoroId y)

-- |
-- User have to create pomodoro with start and end
-- to ensure pomodoro done.
--
-- 割り込みの表現がむずかしい
--
-- pomodoroはdoneかinterruptedしかない
-- もし開始時刻だけを許容してデータ登録させると
-- クラッシュ時に終了時刻が入らないままとなり
-- 中間状態で放置される。
--
-- pomodoroが登録されるのは完了した時、または明示的に割り込み処理された時。
-- それ以外は記録されない、が正解な気がする。
-- 割り込みの有無は割り込みのreason有無で表現できるので
-- pomodoro側に何か状態を保持する必要はない
--
data PomodoroEvent a where
  NewPomodoro :: DateTime -> DateTime -> IssueId -> PomodoroEvent PomodoroId
  RemovePomodoro :: PomodoroId -> PomodoroEvent ()
