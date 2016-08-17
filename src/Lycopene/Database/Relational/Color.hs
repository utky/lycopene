{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Lycopene.Database.Relational.Color where

import           Database.Relational.Query
import           Lycopene.Database.Relational.TH (defineRelationFromDB)

$(defineRelationFromDB "color")


