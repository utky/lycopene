-- | Navigation contains
--  project list
module Nav exposing (..)

import Html exposing (Html, nav, ul, li, a, text)
import Html.Attributes exposing (class, href)
import Project

type alias Nav
  = { items : List NavItem }

type NavItem = NavItem
  { name : String
  , children : List NavItem
  }

type Msg
  = NoOp

view : Project.Projects -> Html Msg
view projects =
  nav
    []
    [ ul
        [ class "nav" ]
        [ li
            []
            [ a
                [ href "#" ]
                [ text "Lycopene" ]
            ]
         , li
            []
            [ a
                [ href "#", class "disabled" ]
                [ text "Projects" ]
            , viewItems projects
            ]
        ]
    ]

viewItems : Project.Projects -> Html Msg
viewItems projects =
  ul
    [ class "nav" ]
    (List.map
      (viewItem projects.focusProject)
      projects.projects)

viewItem : Maybe Project.Project -> Project.Project -> Html Msg
viewItem focus project =
  let
    isActive =
      case focus of
        (Just focused) -> focused.id == project.id
        Nothing -> False
    classes = if isActive then [ class "active" ] else []
  in
    li
      classes
      [ a
          [ href "#" ]
          [ text project.name ]
      ]
  
update : Msg -> Project.Projects -> ( Project.Projects, Cmd Msg )
update msg projects =
  case msg of
    NoOp -> ( projects, Cmd.none )
