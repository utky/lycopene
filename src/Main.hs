module Main
        ( main
        ) where

import Lycopene.Option

main :: IO ()
main = parseLycoCommand >>= runLycoCommand 
