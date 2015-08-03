module Lycopene.Action.Init where

import           Data.Maybe (fromMaybe)
import           Control.Monad.Trans (liftIO)
import           System.FilePath (splitFileName, splitDirectories)
import           Lycopene.Action
import qualified Lycopene.Core.Sprint as Sprint


initialize :: Maybe String -> Maybe String -> FilePath -> Action Integer
initialize n d p = domain $ do
    let dirname = last . splitDirectories . fst . splitFileName
        name = fromMaybe (dirname p) n
    (pjid, _) <- Sprint.newProjectAndSprint name d
    liftIO $ writeFile p (show pjid)
    return pjid
