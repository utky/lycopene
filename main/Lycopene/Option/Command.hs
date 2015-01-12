module Lycopene.Option.Command
    ( LycoCommand (..)
    , LycoAction (..)
    , runLycoCommand
    , mkAction
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe)
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.Environment (lookupEnv)

import           Lycopene.Core
import           Lycopene.Option.Common
import           Lycopene.Configuration

{- Purityを維持するというfunctional wayがすごく難しいと感じる。
-- Computation と IO を分けるという考え方を「どう実践するか」が壁になっている
-- 根本的にDBアクセスをCore DomainというPureな世界の中で可能にしているのが
-- 問題
-- ただ、ある計算を組み上げるだけならpureなはず
-- あまりYesodっぽく考えない方がいいのかもしれない
--
-- ドメイン上での計算を経由して何らかの
--
-- -}

data LycoCommand = LycoCommand CommonOption LycoAction

newtype LycoAction = LycoAction { runAction :: LycopeneT IO () }


mkAction :: Show a => LycopeneT IO a -> Parser LycoAction
mkAction m = pure $ LycoAction printOutput where
  printOutput = m >>= liftIO . print

                  
type HomePath = FilePath

home :: FilePath
home = "~" ++ [pathSeparator]

replaceHome :: FilePath -> HomePath -> FilePath
replaceHome fp hp
  | (firstdir fp) == home = hp </> (tailPath fp)
  | otherwise = fp
  where firstdir = head . splitPath
        tailPath = joinPath . tail . splitPath


runLycoCommand :: LycoCommand -> IO ()
runLycoCommand (LycoCommand c ac) = config >>= (runWithConfiguration ac) where
  runWithConfiguration = runLycopeneT . runAction
  config = fmap configure $ (commonOptsWithHome c) <$> getHomeDirectory
  commonOptsWithHome commonops hp = commonops { homeLocation = expandHome commonops hp }
  expandHome commonops hp = replaceHome (homeLocation commonops) hp


