module Lycopene.Resource where

import           Lycopene.Option (LycoCommand(..), CommonOption(..), Command(..), commonOptsWithHome)
import           Lycopene.Configuration (Configuration(..))
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.Trans (lift, liftIO)
import           Control.Monad.Trans.Error (ErrorT(..), Error(..))
import           System.FilePath
import           System.Directory (doesFileExist, doesDirectoryExist, getCurrentDirectory, getHomeDirectory)

data RsrcError = DoesNotExist 

-- | TODO: すごく適当なインスタンス化
instance Error RsrcError where
  noMsg = DoesNotExist
  strMsg _ = DoesNotExist

configResource :: LycoCommand -> Rsrc Configuration
configResource (LycoCommand co _) = 
  let abshome = expandHome <$> here <*> (selectDirectory . homeLocation $ co)
      lycoH = abshome
      dpath = abshome >>= selectFile . (</> "issues.db")
      localcfg  = (</> ".lyco.conf") <$> here
  in Configuration <$> lycoH <*> dpath

type Rsrc = ErrorT RsrcError IO

select :: (Functor m) => (a -> m Bool) -> a -> ErrorT RsrcError m a
select f a = ErrorT $ (flip ifEither a) <$> f a

selectFile :: FilePath -> Rsrc FilePath
selectFile f = select doesFileExist f 

selectDirectory :: FilePath -> Rsrc FilePath
selectDirectory d = select doesDirectoryExist d 

here :: Rsrc FilePath
here = liftIO getCurrentDirectory

home :: Rsrc FilePath
home = liftIO getHomeDirectory

allocate :: Rsrc a -> IO (Either RsrcError a)
allocate = runErrorT

-- | map bool to maybe
ifEither :: Bool -> a -> Either RsrcError a
ifEither True  = Right
ifEither False = \_ -> Left DoesNotExist


expandHome :: FilePath -> FilePath -> FilePath
expandHome home ('~':xs) = home ++ xs
expandHome home path     = path

