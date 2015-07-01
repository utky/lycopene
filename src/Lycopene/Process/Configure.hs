module Lycopene.Process.Configure where

import           Lycopene.Action
import           Lycopene.Environment (createDatabase)
import           Lycopene.Core
import           Lycopene.Configuration (datapath)
import           Lycopene.Core.Project as Project
import           Lycopene.Core.Sprint as Sprint

-- createParentAndCheckTarget :: (MonadIO m) => FilePath -> Producer Bool m ()
-- createParentAndCheckTarget target = 
--   let createParentIfMissing = (createDirectoryIfMissing True) . dropFileName
--   in liftIO (createParentIfMissing target >> doesFileExist target) >>= yield

-- | Attempt to create fresh database.
configure :: Action String
configure = domain setupSchema

setupSchema :: Lycopene String
setupSchema = createDatabase
            >> Project.inbox
            >> Sprint.inboxDefault
            >> Sprint.inboxBacklog
            >> return "hogehoge" -- FIXME
