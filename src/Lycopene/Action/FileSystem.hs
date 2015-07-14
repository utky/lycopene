module Lycopene.Action.FileSystem where

-- import           Control.Applicative ((<$))
-- import           System.FilePath
import           System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing)

-- | Action over file system.
data FsAction a = IsDir FilePath (Bool -> a)
                | IsFile FilePath (Bool -> a)
                | Read FilePath (String -> a)
                | Write FilePath String a
                | Append FilePath String a
                | MkDir FilePath a

runFsAction :: FsAction a -> IO a
runFsAction (IsDir  p fb ) = fb <$> doesDirectoryExist p
runFsAction (IsFile p fb ) = fb <$> doesFileExist p
runFsAction (Read   p fs ) = fs <$> readFile p
runFsAction (Write  p s x) = x <$ writeFile p s
runFsAction (Append p s x) = x <$ appendFile p s
runFsAction (MkDir  p x  ) = x <$ createDirectoryIfMissing True p

runFsPure :: FsAction a -> Bool -> String -> a
runFsPure (IsDir  _ fb ) b _ = fb b
runFsPure (IsFile _ fb ) b _ = fb b
runFsPure (Read   _ fs ) _ s = fs s
runFsPure (Write  _ _ x) _ _ = x
runFsPure (Append _ _ x) _ _ = x
runFsPure (MkDir  _ x  ) _ _ = x

instance Functor FsAction where
  fmap f (IsDir  p fb ) = IsDir  p (f . fb)
  fmap f (IsFile p fb ) = IsFile p (f . fb)
  fmap f (Read   p fs ) = Read   p (f . fs)
  fmap f (Write  p s x) = Write  p s (f x)
  fmap f (Append p s x) = Append p s (f x)
  fmap f (MkDir  p x  ) = MkDir  p (f x)


isDir :: FilePath -> FsAction Bool
isDir = flip IsDir id

isFile :: FilePath -> FsAction Bool
isFile = flip IsFile id

read :: FilePath -> FsAction String
read = flip Read id

write :: FilePath -> String -> FsAction ()
write p s = Write p s ()

append :: FilePath -> String -> FsAction ()
append p s = Append p s ()

mkdir :: FilePath -> FsAction ()
mkdir = flip MkDir () 

