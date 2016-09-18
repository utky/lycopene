{-# LANGUAGE FlexibleInstances #-}
module Lycopene.Database.Relational.Decode.Sprint where

import           Data.UUID (fromString)
import           Lycopene.Database.Relational.Decode.Prim
import qualified Lycopene.Core as Core
import qualified Lycopene.Database.Relational.Sprint as Sp

sprint :: Decode Sp.Sprint Core.Sprint
sprint =
  Core.Sprint
    <$> decoder (fromString . Sp.sprintId) <?> "sprintId"
    <*> decoder Sp.name
    <*> decoder Sp.description
    <*> decoder (fmap Core.toDate . Sp.startOn)
    <*> decoder (fmap Core.toDate . Sp.endOn)
    <*> decoder (sprintStatus . Sp.status) <?> "projectStatus" 

sprintStatus :: Int -> Maybe Core.SprintStatus
sprintStatus 0 = Just Core.SprintFinished
sprintStatus 1 = Just Core.SprintRunning
sprintStatus _ = Nothing
