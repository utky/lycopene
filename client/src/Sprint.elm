module Sprint exposing (..)

import Html exposing (..)
import Date exposing (Date)

type alias SprintId = String

type alias Sprint =
  { id : SprintId
  , name : String
  , description : Maybe String
  , startOn : Maybe Date
  , endOn : Maybe Date
  , status : String
  }

-- | A focus into a sprint via a project
type alias SprintFocus =
  { projectId : String
  , sprintId : String
  }


type alias Sprints = 
  { sprints : List Sprint
  , focus : Maybe SprintFocus
  }

type Msg
  = NoOp

init : Sprints
init =
  { sprints = []
  , focus = Nothing
  }

find : SprintId -> List Sprint -> Maybe Sprint
find id sprints =
  sprints
    |> List.filter (\s -> id == s.id)
    |> List.head

view : Sprints -> Html Msg
view model =
  let
    sprintBoard =
      case model.focus of
        Just focus -> viewSprintBoard focus model.sprints
        Nothing    -> text "no sprint"
  in
    div
      []
      [ sprintBoard ]

viewSprintBoard : SprintFocus -> List Sprint -> Html Msg
viewSprintBoard focus sprints =
  let
    focused = find focus.sprintId sprints
  in
    case focused of
      Nothing -> text ("no sprint matches in: " ++ focus.sprintId)
      Just s  ->
        div
          []
          [ h6
              []
              [ text s.name ]
          ]

update : Msg -> Sprints -> ( Sprints, Cmd Msg )
update msg model =
  case msg of
    NoOp -> model ! []
