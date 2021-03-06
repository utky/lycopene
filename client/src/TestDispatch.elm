module TestDispatch exposing (..)

import Task
import Api exposing (Dispatch, Req(..), Res(..), pure)

dispatch : Dispatch
dispatch r =
  case r of
    FetchProjects -> 
      let
        pj = 
          { id = "hoge"
          , name = "name"
          , description = Just "desc"
          , status = "active" }
      in
        (FetchProjectsDone [ pj ]) |> pure FetchProjectsFail 
