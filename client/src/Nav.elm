-- | Navigation contains
--  project list
module Nav exposing (..)

import Html as Html exposing (Html, nav, ul, li, a, text)
import Html.Attributes exposing (class, href)
import Data.Product as Product
import Task
import Project
import Sprint

type Msg
  = ProjectMsg Project.Msg
  | SprintMsg Sprint.Msg

view : Project.Model -> Sprint.Model -> Html Msg
view project sprint =
  nav []
    [ ul [ class "nav" ]
        [ li []
            [ a [ href "#", class "disabled" ]
                [ text "Projects" ]
            , Project.view project |> Html.map ProjectMsg
            ]
        , li []
            [ a [ href "#", class "disabled" ]
                [ text "Sprints" ]
            , Sprint.view sprint |> Html.map SprintMsg
            ]
        ]
    ]

update : Msg -> (Project.Model, Sprint.Model) -> ( (Project.Model, Sprint.Model), Cmd Msg )
update msg (project, sprint) =
  case msg of
    ProjectMsg m -> 
      Project.update m project
        |> Product.tmap
          (\n -> (n, sprint))
          (\c -> Cmd.batch [ (Cmd.map ProjectMsg c), handleProjectFocus m ])
    SprintMsg m -> 
      Sprint.update m sprint
        |> Product.tmap
          (\n -> (project, n))
          (Cmd.map SprintMsg)

cmd : m -> Cmd m
cmd msg = Task.perform (always msg) (Task.succeed msg)

handleProjectFocus : Project.Msg -> Cmd Msg
handleProjectFocus msg =
  case msg of
    Project.Focus p ->
      Cmd.map SprintMsg (cmd (Sprint.FetchAll p))
    _ -> Cmd.none
