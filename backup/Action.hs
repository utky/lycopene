module Lycopene.Action where

import           Lycopene.Core
import           Lycopene.Configuration
import           Control.Monad.Except (ExceptT(..), runExceptT)
import           Control.Monad.Reader (ReaderT(..))

import           Lycopene.Action.Domain
import qualified Lycopene.Action.FileSystem as FS
import           Lycopene.Print


-- | An alias represents a result of action or failure.
type ActionResult = ExceptT LycoError IO

-- | Action represents a transaction boudary when it contains domain operations.
-- Action represents specific operations below:
data ActionF a
    --   1. Domain :: Context -> IO (Either Error a)
    = Domain (Domain a)
    --   2. FS :: IO (Either Error a)
    | FsAction (FS.FsAction a)
    --   3. Send :: String -> a
    | Send String a

instance Functor ActionF where
  fmap f (Domain x)   = Domain $ fmap (fmap f) x
  fmap f (FsAction x) = FsAction $ fmap f x
  fmap f (Send s x)   = Send s (f x)


type Action = Free ActionF
        

runWithContext :: Context -> Action a -> ActionResult a
runWithContext ctx action =
  case action of
       (Pure x)             -> actionResult . return . Right $ x
       (Free (FsAction f))  -> runWithContext ctx =<< actionResult (fmap Right (FS.runFsAction f))
       (Free (Domain d))    -> runWithContext ctx =<< actionResult (runReaderT d ctx)
       (Free (Send s x))    -> actionResult (putStrLn s >> (return . Right $ x)) >>= runWithContext ctx

{- Freeモナドを畳み込む場合のaccumlatorについて考える。
 -
 - ここでaccumは実質的に一回のトランザクションというか状態を持ったコネクションのはず
 - Closedならリソースを獲得して計算を行い
 - Openedならすでにあるリソースを利用して計算する
 -
 - この状態を伝搬するのがアキュムレータとなるState相当のデータ
 -
 - StateT (Either Config Context) ActionResult a
 -
 - OR
 -
 - もう少し単純にあるトランザクションという文脈の中で実行されるように
 - 明確に表現されていればいい気がする。
 -
 - FS
 -}

runAction' :: ActionF a ->  ActionResult a
runAction' (FsAction f) = actionResult (fmap Right (FS.runFsAction f))
runAction' (Domain d)   = undefined
runAction' (Send s x)   = actionResult (putStrLn s >> (return . Right $ x))

runAction :: Configuration -> Action a -> ActionResult a
runAction cfg action = 
  case action of
       (Pure x)             -> actionResult . return . Right $ x
       (Free (FsAction f))  -> runAction cfg =<< actionResult (fmap Right (FS.runFsAction f))
       (Free (Domain d))    -> let ctxrs = fetchResource cfg
                                   go = runResource ctxrs $ \ctx ->
                                            withDataSourceTx (dataSource ctx) $ \conn ->
                                              let newCtx = ctx { dataSource = mkDataSource conn }
                                              in handleResult (runWithContext newCtx =<< actionResult (runReaderT d newCtx))
                               in actionResult go
       (Free (Send s x))    -> actionResult (putStrLn s >> (return . Right $ x)) >>= runAction cfg


actionResult :: IO (Either LycoError a) -> ActionResult a
actionResult = ExceptT

handleResult :: ActionResult a -> IO (Either LycoError a)
handleResult = runExceptT

-- handler for response to UI

send :: (Print a) => a -> Action ()
send = liftF . flip Send () . printA

-- Lifter for FS utilitiy

liftFS :: FS.FsAction a -> Action a
liftFS = liftF . FsAction

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

-- Lifter for Domain utility

domain :: Lycopene a -> Action a
domain = liftF . Domain . mapDomain
