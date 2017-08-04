module Sprint exposing (..)

import Html exposing (Html, div, ul, li, h6, text, a)
import Html.Attributes exposing (class, placeholder, type_, href, value, style)
import Html.Events exposing (onInput, onClick)
import Date exposing (Date)
import Project

type alias SprintId = String

type alias Sprint =
  { id : SprintId
  , name : String
  , description : Maybe String
  , startOn : Maybe Date
  , endOn : Maybe Date
  , status : String
  }

-- | 
type alias Model = 
  { sprints : List Sprint
  , focus : Maybe SprintId
  }

type Msg
  = FetchAll Project.ProjectId
  | Load (List Sprint)
  | Focus SprintId

init : Model
init =
  { sprints = []
  , focus = Nothing
  }

find : SprintId -> List Sprint -> Maybe Sprint
find id sprints =
  sprints
    |> List.filter (\s -> id == s.id)
    |> List.head

view : Model -> Html Msg
view model =
  div []
    [ viewSprints model ]

viewSprints : Model -> Html Msg
viewSprints model =
  List.map (viewItem model.focus) model.sprints |> ul []
 
  
viewItem : Maybe SprintId -> Sprint -> Html Msg
viewItem focus sprint =
  let
    styles =
      case focus of
        Nothing -> []
        Just id ->
          if id == sprint.id then [("color", "red")] else []

  in
    li [style styles]
      [ a [ href "#", onClick (Focus sprint.id) ]
          [ text sprint.name ]
      ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Load s ->
      { model | sprints = s } ! []
    Focus id ->
      { model | focus = Just id } ! []
    -- FIXME: cases not exhausitive
    otherwise -> model ! []

decodeSprints : Decoder (List Sprint)
decodeSprints = list decodeSprint

decodeSprint : Decoder Sprint
decodeSprint =
  Sprint
    |$| field "id" string
    |*| field "name" string
    |*| field "description" (maybe string)
    |*| field "startOn" string
    |*| field "endOn" string
    |*| field "status" string
