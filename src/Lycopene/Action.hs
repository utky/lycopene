module Lycopene.Action where

import           Lycopene.Core
import           Lycopene.Configuration
import           Control.Monad.Except (ExceptT(..), runExceptT)
import           Control.Monad.Reader (ReaderT(..))
import           Database.HDBC (withTransaction)

import           Lycopene.Action.Domain
import qualified Lycopene.Action.FileSystem as FS
import           Lycopene.Print


-- | 
type ActionResult = ExceptT LycoError IO

-- | Action represents a transaction boudary when it contains domain operations.
-- Action encloses specific operations below:
--   1. Domain :: Context -> IO (Either Error a)
--   2. FS :: IO (Either Error a)
--   3. Pure (provided by Free)
data ActionF a = MkDomain (Domain a) | MkFsAction (FS.FsAction a) | Send String a

instance Functor ActionF where
  fmap f (MkDomain x)   = MkDomain $ fmap (fmap f) x
  fmap f (MkFsAction x) = MkFsAction $ fmap f x
  fmap f (Send s x)   = Send s (f x)


type Action = Free ActionF
        

runWithContext :: Context -> Action a -> ActionResult a
runWithContext ctx action =
  case action of
       (Pure x)               -> actionResult . return . Right $ x
       (Free (MkFsAction f))  -> runWithContext ctx =<< actionResult (fmap Right (FS.runFsAction f))
       (Free (MkDomain d))    -> runWithContext ctx =<< actionResult (runReaderT d ctx)
       (Free (Send s x))      -> actionResult (putStrLn s >> (return . Right $ x)) >>= runWithContext ctx

runAction :: Configuration -> Action a -> ActionResult a
runAction cfg action = 
  let handleFree = (>>= runAction cfg)
  in case action of
       (Pure x)               -> actionResult . return . Right $ x
       (Free (MkFsAction f))  -> runAction cfg =<< actionResult (fmap Right (FS.runFsAction f))
       (Free (MkDomain d))    -> let ctxrs = fetchResource cfg
                                     go = runResource ctxrs $ \ctx ->
                                            withDataSourceTx (dataSource ctx) $ \conn ->
                                              let newCtx = ctx { dataSource = mkDataSource conn }
                                              in handleResult (runWithContext newCtx =<< actionResult (runReaderT d newCtx))
                                 in actionResult go
       (Free (Send s x))      -> actionResult (putStrLn s >> (return . Right $ x)) >>= runAction cfg


actionResult :: IO (Either LycoError a) -> ActionResult a
actionResult = ExceptT

handleResult :: ActionResult a -> IO (Either LycoError a)
handleResult = runExceptT

-- handler for response to UI

send :: (Print a) => a -> Action ()
send = liftF . flip Send () . printA
 

--

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
