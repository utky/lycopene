module Lycopene.Option.Init (initLyco) where


import           Options.Applicative
import           System.FilePath
import           System.Directory

import           Lycopene.Core
import           Lycopene.Option.Command



initLyco :: ParserInfo LycoAction
initLyco = info initP (progDesc "Initialize a directory as Lyco base")
  where
    initP = mkAction runInit
    runInit = liftL initLycoDir

initLycoDir :: IO FilePath
initLycoDir = do
  fmap (\d -> d </> ".lyco") getCurrentDirectory

