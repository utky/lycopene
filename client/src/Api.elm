module Api exposing (..)

import Http
import Task
import Project

type alias Error = Http.Error

type Msg
  = Rq Req
  | Rs Res

type Req
  = FetchProjects

type Res
  = GenericError Error
  | FetchProjectsDone (List Project.Project)
  | FetchProjectsFail Error

type alias Dispatch
  = Req -> Cmd Res

runDispatch : Dispatch -> Req -> Cmd Msg
runDispatch dispatch req = dispatch req |> Cmd.map Rs

req : Req -> Cmd Msg
req rq = pure (Rs << GenericError) (Rq rq)

pure : (e -> a) -> a -> Cmd a
pure fail value =
  Task.succeed value
    |> Task.perform fail identity

