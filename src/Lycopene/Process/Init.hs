module Lycopene.Process.Init where

import           System.FilePath (splitFileName, (</>))
import           System.Directory (getCurrentDirectory)
import           System.IO (writeFile)
import           Lycopene.Process.Core
import           Lycopene.Resource (expandCurrent)
import qualified Lycopene.Core.Project as Project
import qualified Lycopene.Core.Sprint as Sprint

initialize :: (MonadIO m) => (Maybe String) -> (Maybe String) -> FilePath -> ProcessR m
initialize n d p = do
  path <- liftIO $ (flip expandCurrent) p <$> getCurrentDirectory
  let (_, dirname) = splitFileName path
      name = case n of
               (Just nameStr) -> nameStr
               Nothing        -> dirname
  (pjid, _) <- runDomain $ Sprint.newProjectAndSprint name d
  liftIO $ writeFile (path </> ".lyco.config") (show pjid)
  complete
   

