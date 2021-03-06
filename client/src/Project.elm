module Project exposing (..)

import Html exposing (..)
import Http
import Json.Decode exposing (Decoder, object4, string, maybe, list, (:=))

type alias ProjectId = String
type alias Name = String

type alias Project =
  { id : ProjectId
  , name : Name
  , description : Maybe String
  , status : String
  }

type alias Projects =
  { projects : List Project
  , focusProject : Maybe Project
  }

type Msg
  = NoOp
  | FocusProject ProjectId


init : Projects
init = { projects = [], focusProject = Nothing }


view : Projects -> Html Msg
view { projects } =
  div []
      [ ul
          []
          (List.map viewItem  projects)
      ]
      
viewItem : Project -> Html Msg
viewItem { name } = li [] [ text name ]

update : Msg -> Projects -> ( Projects, Cmd Msg )
update msg projects =
  case msg of
    NoOp -> projects ! []
    FocusProject id ->
      let
        focus = 
          List.filter (\p -> p.id == id) projects.projects
            |> List.head
      in
        ({ projects | focusProject = focus }, Cmd.none)

decodeProjects : Decoder (List Project)
decodeProjects = list decodeProject

decodeProject : Decoder Project
decodeProject =
  object4 Project
    ("id" := string)
    ("name" := string)
    ("description" := maybe string)
    ("status" := string)

