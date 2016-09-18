{-# LANGUAGE MultiParamTypeClasses #-}
module Lycopene.Database.Relational.Decode.Class where

import           Lycopene.Database.Relational.Decode.Prim
import           Lycopene.Database.Relational.Decode.Project (project)
import           Lycopene.Database.Relational.Decode.Issue (issue)
import           Lycopene.Database.Relational.Decode.Sprint (sprint)

import qualified Lycopene.Core as Core
import qualified Lycopene.Database.Relational.Project as ProjectR
import qualified Lycopene.Database.Relational.Sprint as SprintR
import qualified Lycopene.Database.Relational.Issue as IssueR


class Decoder a b where

  defDecoder :: Decode a b

  decode :: a -> DecodeResult b
  decode = execDecode defDecoder


instance Decoder ProjectR.Project Core.Project where
  defDecoder = project

instance Decoder SprintR.Sprint Core.Sprint where
  defDecoder = sprint

instance Decoder IssueR.Issue Core.Issue where
  defDecoder = issue
