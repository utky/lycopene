module Dispatch exposing (..)

import Http
import Api
import App
import Nav
import Project
import Sprint

send : (a -> App.Msg) -> Http.Request a -> Cmd App.Msg
send = Http.send << Api.fold App.ApiError 

perform : Api.Handler App.Msg
perform msg =
  case msg of
    App.NavMsg (Nav.ProjectMsg (Project.FetchAll)) ->
      Http.get  "/projects" Project.decodeProjects
        |> send (App.NavMsg << Nav.ProjectMsg << Project.Load)
        |> Api.Done

    App.NavMsg (Nav.ProjectMsg (Project.Submit newProject)) ->
      Http.post "/projects" (Http.jsonBody (Project.encodeNew newProject)) Project.decodeProject
        |> send (App.NavMsg << Nav.ProjectMsg << Project.Add)
        |> Api.Done

    App.NavMsg (Nav.SprintMsg (Sprint.FetchAll projectId)) ->
      Http.get ("/sprints?project=" ++ projectId) Sprint.decodeSprints
        |> send (App.NavMsg << Nav.SprintMsg << Sprint.Load)
        |> Api.Done

    otherwise -> Api.Pass otherwise
