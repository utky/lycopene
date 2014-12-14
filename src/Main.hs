module Main
        ( main
        ) where

import System.Environment
--import System.Console.Haskeline
import Lycopene.Option

main :: IO ()
main = getArgs >>= runWithArguments

runWithArguments :: [String] -> IO ()
--runWithArguments []          = interactive
runWithArguments args = do
    case  parseOptions args of
      Just c   -> runLycoCommand c
      Nothing  -> putStrLn "Invalid parameter"
{-
interactive :: IO ()
interactive = runInputT defaultSettings loop where
    loop = getInputLine prompt >>= consumeInput
    consumeInput Nothing       = return ()
    consumeInput (Just "quit") = return ()
    consumeInput (Just input)  = execute input >> loop
    execute = return . fmap runLycoCommand . parseOptions . arguments
    arguments = return
    prompt = "todo> "
-}
