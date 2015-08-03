module Lycopene.Option.Rd (ready) where


import           Options.Applicative
import           Lycopene.Option.Command


ready :: ParserInfo Command
ready = 
  let rdP = Rd <$> option auto (long "issue" <> short 'i')
  in info rdP (progDesc "Set specified issue ready to solve.")


