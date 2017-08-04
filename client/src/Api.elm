module Api exposing (..)

import Http
import Result as Result exposing (Result(..))

type alias Handler a = a -> Api a
type Api a = Done (Cmd a) | Next a (Cmd a) | Pass a
type alias Error = Http.Error

fold : (e -> m) -> (a -> m) -> Result e a -> m
fold f g r =
  case r of
    Err x -> f x
    Ok y  -> g y
