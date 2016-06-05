module Lycopene.Core.Sandbox where

{- | 
 -
 -}

data IssueStatus
    = New
    | Ready
    | Progress
    | Done

{- | An Issue represents a task 
 - This is an entity identified by issue id.
 -}
type IssueId = Integer
data Issue = 
    { issueId     :: IssueId
    , title       :: Text
    , description :: Maybe Text
    , status      :: IssueStatus
    , priority    :: Priority
    , dueOn       :: Maybe Date
    , startOn     :: Maybe Date
    , endOn       :: Maybe Date
    , creatOn     :: Date
    , timeTracks  :: [TimeTrack]
    }

{- | A time record created on each pomodoro time box 
 -
 -}
data TimeTrack =
    { trackStart :: Timestamp
    , trackEnd   :: Timestamp
    }

data Sprint =
    { name         :: Text
    , description  :: Maybe Text
    , dueOn        :: Maybe Date
    , sprintStatus :: 
    , issues      :: [Issue]
    }

{- | A unit of activity to 
 - which contians some issues.
 -
 -}
data Project =
    { name        :: String
    , description :: Maybe Text
    , creatOn     :: Date
    , sprints     :: [Sprint]
    }

{- | A time box 
 -
 -}
data TimeBox =
    { interval :: Minute
    , timeLeft :: Second
    , workOn   :: Issue
    }

-------------------------------
-- Control
-------------------------------
--

-- | データaを永続化せよという命令
-- ただし
-- a -> Entity b
-- となるような関数が存在している必要がある
data Persist a

-- | データaを永続化ストアから取り出す命令
data Fetch a

-- | 永続化ストアより取り出されたデータa
data Store a


data IssueState
    = New
    | Ready
    | Work
    | Done
    | Reject

data IssueEvent
    = Create -- ^ 
    | Modify -- ^ Modify Issue data in-place
    | Fetch  -- ^ Get issue from backlog and ready to start
    | Start  -- ^ Start to try solving issue.
    | Done   -- ^ Dont need to try it no more.
    | AddTimeTrack -- ^ リレーションの変更

data TimeTrackState
    = Start  -- ^ Recorded timestamp on start but end time.
    | Finish -- ^ Recorded end time.

data TimeTrackEvent
    = Start     -- ^ 
    | Finish
    | Interrupt

data SprintState
    = Open    -- ^ Opened sprint can be added more issues.
    | Cancell -- ^ Cancelled sprint is regarded that there is no output to metric.
    | Close   -- ^ Closed sprint is successfuly completed.

data SprintEvent
    = New
    | Cancell
    | Close
    | AddIssue    -- ^ 注意: リレーションの変更
    | RemoveIssue -- ^ 注意: リレーションの変更
    | ModifyDescr

data ProjectState -- same as sprint
    = Open
    | Cancell
    | Close

data ProjectEvent
    = New
    | Cancell
    | Close
    | AddSprint    -- ^ 注意: リレーションの変更
    | RemoveSprint -- ^ 注意: リレーションの変更
    | ModifyDescr

