module Lycopene.Option.Version (version) where


import           Options.Applicative

import           Lycopene.Option.Command


version :: ParserInfo Command
version = info (pure (Administration Version)) (progDesc "Print appilcation version infomation")

