module Lycopene.TH (str) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
 
str :: QuasiQuoter
str = QuasiQuoter { quoteExp = stringE }