module Lycopene.Process.New where

import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (ask)
import qualified Data.Text as T
import           Lycopene.Process.Internal
import           Lycopene.Configuration
import           Lycopene.Core
import           Lycopene.Core.Issue as Issue


{-
newIssue :: (MonadIO m) => String -> Maybe String -> ProcessR m
newIssue title mDesc = do
  pjId <- targetProject `fmap` lift ask
  _ <- runDomain $ Issue.newIssue IssueRequest 
                                  { irTitle = title
                                  , irDescription = mDesc
                                  , irProjectId = pjId
                                  }
  complete
-}
newIssue = undefined
