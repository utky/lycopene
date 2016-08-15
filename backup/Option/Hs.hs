module Lycopene.Option.Hs (recordHistory) where


import           Options.Applicative
import           Lycopene.Option.Command
import           Lycopene.Option.Internal


recordHistory :: ParserInfo Command
recordHistory = 
  let hsP = Hs <$> argid
  in info hsP (progDesc "list history of records")

