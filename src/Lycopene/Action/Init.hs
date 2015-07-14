module Lycopene.Action.Init where

import           System.FilePath (splitFileName, (</>))
import           System.Directory (getCurrentDirectory)
import           Lycopene.Action
import           Lycopene.Process.Internal
import           Lycopene.Resource (expandCurrent)
import qualified Lycopene.Core.Sprint as Sprint


initialize :: Maybe String -> Maybe String -> FilePath -> Action Integer
initialize n d p = domain $ do
    path <- liftIO $ flip expandCurrent p <$> getCurrentDirectory
    let (_, dirname) = splitFileName path
        name = case n of
                 (Just nameStr) -> nameStr
                 Nothing        -> dirname
    (pjid, _) <- Sprint.newProjectAndSprint name d
    liftIO $ writeFile (path </> ".lyco.config") (show pjid)
    return pjid
