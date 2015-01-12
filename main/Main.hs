module Main
        ( main
        ) where

import Options.Applicative
import Lycopene.Option

main :: IO ()
main = parseLycoCommand >>= runLycoCommand where
  parseLycoCommand = execParser lycoParser
