{-# LANGUAGE GADTs #-}
module Lycopene.Core.Record where

import           Lycopene.Core.Scalar
import           Lycopene.Core.Issue (IssueId)

type RecordId = Identifier

data Record
    = Record
    { recordId :: !Identifier
    , recordStartOn :: !DateTime
    , recordEndOn :: !(Maybe DateTime)
    } deriving (Show)

instance Eq Record where
    x == y = (recordId x) == (recordId y)

-- |
-- User have to create record with start and end
-- to ensure record done.
--
-- 割り込みの表現がむずかしい
--
-- recordはdoneかinterruptedしかない
-- もし開始時刻だけを許容してデータ登録させると
-- クラッシュ時に終了時刻が入らないままとなり
-- 中間状態で放置される。
--
-- recordが登録されるのは完了した時、または明示的に割り込み処理された時。
-- それ以外は記録されない、が正解な気がする。
-- 割り込みの有無は割り込みのreason有無で表現できるので
-- record側に何か状態を保持する必要はない
--
data RecordEvent a where
  NewRecord :: DateTime -> DateTime -> IssueId -> RecordEvent RecordId
  RemoveRecord :: RecordId -> RecordEvent ()
