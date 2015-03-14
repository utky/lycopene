module Lycopene.Option.Resource
    ( Store(..)
    , Resource(..)
    , mkResource
    ) where

import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Applicative
import           System.FilePath
import           System.Directory (doesFileExist)

newtype Store = Store { unStore :: FilePath }

data Resource = Resource
                     { store :: Maybe Store
                     , currentProject :: Maybe Integer
                     }

mkResource :: FilePath -> FilePath -> IO Resource
mkResource hd cd = Resource
                     <$> userStore
                     <*> (readProjectId lycofile) where
    userStore = runMaybeT $ fileMaybe (hd </> ".lyco") >>= lift . return . Store
    lycofile = cd </> ".lyco"


readProjectId:: FilePath -> IO (Maybe Integer)
readProjectId path = runMaybeT $ fileMaybe path >>=
  lift . readFile >>= 
  MaybeT . return . readHeadLine 

fileMaybe :: FilePath -> MaybeT IO FilePath
fileMaybe a = MaybeT $ (flip ifMaybe a) <$> doesFileExist a

-- | map bool to maybe
ifMaybe :: Bool -> a -> Maybe a
ifMaybe True  = Just
ifMaybe False = \_ -> Nothing
  
-- | take a head of list safely
head' :: [a] -> Maybe a
head' (x:_) = Just x
head' []     = Nothing

readHeadLine :: Read a => String -> Maybe a
readHeadLine  = fmap read .  head' . lines
