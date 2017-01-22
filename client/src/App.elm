module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Style
import Nav
import Project
import Issue
import Sprint
import Route
import Api

-- MODEL

type alias AppState =
  { route : Route.Route
  , projects : Project.Projects
  , issues : Issue.Issues
  , sprints : Sprint.Sprints
  }


init : ( AppState, Cmd Msg )
init =
  let
    model =
      { route = Route.Dashboard
      , projects = Project.init
      , issues = Issue.init
      , sprints = Sprint.init
      }
    cmd = Cmd.map ApiMsg (Api.req Api.FetchProjects)
  in
    ( model, cmd )


-- MESSAGES


type Msg
  = NoOp
  | ApiMsg Api.Msg
  | NavMsg Nav.Msg
  | ProjectMsg Project.Msg
  | IssueMsg Issue.Msg
  | SprintMsg Sprint.Msg

-- VIEW

view : AppState -> Html Msg
view model =
  div [ ]
      [ Style.normalize
      , Style.skeleton
      , Style.custom
      , div [ class "row" ]
          [ div [ class "two columns" ]
              [ Html.App.map NavMsg (Nav.view model.projects)
              ]
          , div [ class "ten columns" ]
              [ Html.App.map SprintMsg (Sprint.view model.sprints)
              ]
          ]
      ]

-- UPDATE


update : Api.Dispatch -> Msg -> AppState -> ( AppState, Cmd Msg )
update dispatch msg model =
  case msg of

    NoOp -> model ! []

    ApiMsg m ->
      case m of
        Api.Rq r -> (model, Api.runDispatch dispatch r |> Cmd.map ApiMsg)
        Api.Rs r -> 
          case r of
            -- TODO: where should I handle response
            (Api.GenericError e) -> model ! [] 
            (Api.FetchProjectsDone pjs) -> 
              let
                projects = model.projects
              in
                ({ model | projects = { projects | projects = pjs } }, Cmd.none)
            (Api.FetchProjectsFail e) -> model ! []

    NavMsg m -> 
      let (n, c) = Nav.update m model.projects
      in  ({ model | projects = n }, Cmd.map NavMsg c)

    ProjectMsg m ->
      let (n, c) = Project.update m model.projects
      in  ({ model | projects = n }, Cmd.map ProjectMsg c)

    IssueMsg m ->
      let (n, c) = Issue.update m model.issues
      in  ({ model | issues = n}, Cmd.map IssueMsg c)

    SprintMsg m ->
      let (n, c) = Sprint.update m model.sprints
      in  ({ model | sprints = n}, Cmd.map SprintMsg c)



-- SUBSCRIPTIONS


subscriptions : AppState -> Sub Msg
subscriptions model =
  Sub.none

