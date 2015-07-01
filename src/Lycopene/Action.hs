module Lycopene.Action where

import           Lycopene.Core
import           Lycopene.Configuration
import           Control.Monad.Except (ExceptT(..), runExceptT)

import           Lycopene.Action.Domain
import qualified Lycopene.Action.FileSystem as FS

-- | 
type ActionResult = ExceptT LycoError IO

-- | Action represents a transaction boudary when it contains domain operations.
-- Action encloses specific operations below:
--   1. Domain :: Context -> IO (Either Error a)
--   2. FS :: IO (Either Error a)
--   3. Pure (provided by Free)
data ActionF a = MkDomain (Domain a) | MkFsAction (FS.FsAction a)

instance Functor ActionF where
  fmap f (MkDomain x)   = MkDomain $ fmap (fmap f) x
  fmap f (MkFsAction x) = MkFsAction $ fmap f x

type Action = Free ActionF

runAction :: Action a -> Configuration -> ActionResult a
runAction m cfg = let handleFree x = actionResult x >>= flip runAction cfg
                  in  case m of
                    (Pure x)               -> actionResult . return . Right $ x
                    (Free (MkDomain d))    -> handleFree (runDomain d (fetchResource cfg))
                    (Free (MkFsAction f))  -> handleFree (fmap Right (FS.runFsAction f))

actionResult :: IO (Either LycoError a) -> ActionResult a
actionResult = ExceptT

handleResult :: ActionResult a -> IO (Either LycoError a)
handleResult = runExceptT

liftFS :: FS.FsAction a -> Action a
liftFS = liftF . MkFsAction

-- File system action

isDir :: FilePath -> Action Bool
isDir = liftFS . FS.isDir

isFile :: FilePath -> Action Bool
isFile = liftFS . FS.isFile

mkdir :: FilePath -> Action ()
mkdir = liftFS . FS.mkdir

read :: FilePath -> Action String
read = liftFS . FS.read

write :: FilePath -> String -> Action ()
write = curry $ liftFS . uncurry FS.write

append :: FilePath -> String -> Action ()
append = curry $ liftFS . uncurry FS.append

--

domain :: Lycopene a -> Action a
domain = liftF . MkDomain . mapDomain
