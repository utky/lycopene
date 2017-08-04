module App exposing (..)

import Html exposing (Html, map, div, text)
import Html.Attributes exposing (class)
import Http
import Style
import Nav
import Project
import Issue
import Sprint
import Route
import Api
import Data.Product as Product
import Internal exposing (promote)

-- MODEL

type alias State =
  { route : Route.Route
  , projects : Project.Model
  , issues : Issue.Issues
  , sprints : Sprint.Model
  }


init : ( State, Cmd Msg )
init =
  let
    model =
      { route = Route.Dashboard
      , projects = Project.init
      , issues = Issue.init
      , sprints = Sprint.init
      }
    cmd = Project.FetchAll
            |> Nav.ProjectMsg
            |> NavMsg
            |> promote
  in
    ( model, cmd )


-- MESSAGES


type Msg
  = ApiError Api.Error
  | NavMsg Nav.Msg
  | ProjectMsg Project.Msg
  | IssueMsg Issue.Msg
  | SprintMsg Sprint.Msg

-- VIEW

view : State -> Html Msg
view model =
  div [ ]
      [ Style.normalize
      , Style.skeleton
      , Style.custom
      , div [ class "row" ]
          [ div [ class "two columns" ]
              [ Nav.view model.projects model.sprints |> map NavMsg 
              ]
          , div [ class "ten columns" ]
              [ text "issues"
              ]
          ]
      ]

-- UPDATE


update : Api.Handler Msg -> Msg -> State -> ( State, Cmd Msg )
update handler msg model =
  case handler msg of
    Api.Done cmd -> model ! [cmd]
    Api.Next m cmd ->
      let
        (mdl, later) = updateState m model
      in
        mdl ! [cmd, later]
    Api.Pass m -> updateState m model


updateState : Msg -> State -> ( State, Cmd Msg )
updateState msg model =
  case msg of
    ApiError e -> model ! []
    NavMsg m -> 
      Nav.update m (model.projects, model.sprints)
        |> Product.tmap
          (\n -> { model | 
                   projects = Tuple.first n,
                   sprints = Tuple.second n })
          (Cmd.map NavMsg)

    ProjectMsg m ->
      Project.update m model.projects
        |> Product.tmap
          (\n -> { model | projects = n })
          (Cmd.map ProjectMsg)

    IssueMsg m ->
      Issue.update m model.issues
        |> Product.tmap
          (\n -> { model | issues = n})
          (Cmd.map IssueMsg)

    SprintMsg m ->
      Sprint.update m model.sprints
        |> Product.tmap
          (\n -> { model | sprints = n})
          (Cmd.map SprintMsg)


-- SUBSCRIPTIONS


subscriptions : State -> Sub Msg
subscriptions model = Sub.none

