{-# LANGUAGE MultiParamTypeClasses #-}
module Lycopene.Database.Relational.Decode.Class where

import           Lycopene.Database.Relational.Decode.Prim
import           Lycopene.Database.Relational.Decode.Project (project)

import qualified Lycopene.Core as Core
import qualified Lycopene.Database.Relational.Project as RProject


class Decoder a b where

  defDecoder :: Decode a b

  decode :: a -> DecodeResult b
  decode = execDecode defDecoder


instance Decoder RProject.Project Core.Project where
  defDecoder = project
