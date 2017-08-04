module TestDispatch exposing (..)

import Api
import App
import Nav
import Project
import Sprint
import Internal exposing (promote)

perform : Api.Handler App.Msg
perform msg =
  case msg of
    App.NavMsg (Nav.ProjectMsg (Project.FetchAll)) ->
      let
        pj = 
          { id = "hoge"
          , name = "name"
          , description = Just "desc"
          , status = "active" }
      in
        Project.Load [pj]
          |> Nav.ProjectMsg 
          |> App.NavMsg
          |> promote
          |> Api.Done
    App.NavMsg (Nav.ProjectMsg (Project.Submit p)) ->
      let
        pj = 
          { id = "new"
          , name = p.name
          , description = Just p.description
          , status = "active" }
      in
        Project.Add pj
          |> Nav.ProjectMsg 
          |> App.NavMsg
          |> promote
          |> Api.Done
    App.NavMsg (Nav.SprintMsg (Sprint.FetchAll p)) ->
      let
        sp = 
          { id = "new-sprint"
          , name = "sprint-name"
          , description = Nothing
          , status = "active"
          , startOn = Nothing
          , endOn = Nothing
          }
      in
        Sprint.Load [sp]
          |> Nav.SprintMsg 
          |> App.NavMsg
          |> promote
          |> Api.Done
    otherwise -> Api.Pass otherwise


