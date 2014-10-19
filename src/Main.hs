module Main
        ( main
        ) where

import System.Console.Haskeline
import Lycopene.Option

main :: IO ()
main = runInputT defaultSettings loop where
    loop = getInputLine prompt >>= consumeInput
    consumeInput Nothing       = return ()
    consumeInput (Just "quit") = return ()
    consumeInput (Just input)  = execute input >> loop
    execute = outputStrLn . show . parseOptions . arguments
    arguments = return
    prompt = "> "

