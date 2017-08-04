module Project exposing (..)

import Html exposing (Html, div, ul, li, a, text, form, input, button)
import Html.Attributes exposing (class, placeholder, type_, href, value, style, action)
import Html.Events exposing (onInput, onClick, onSubmit)
import Http
import Json.Decode exposing (Decoder, string, maybe, list, field)
import Json.Decode.Applicative exposing (..)
import Json.Encode as Encode
import Internal exposing (pure)

type alias ProjectId = String
type alias Name = String

type alias Project =
  { id : ProjectId
  , name : Name
  , description : Maybe String
  , status : String
  }

type alias NewProject = 
  { name : Name
  , description : String
  }

type alias Model =
  { projects : List Project
  , focus : Maybe ProjectId
  , new : NewProject
  }

type Msg
  = FetchAll
  | Load (List Project)
  | Focus ProjectId
  | Name String
  | Desc String
  | Submit NewProject
  | Add Project


init : Model
init = { projects = [], focus = Nothing, new = NewProject "" "" }


view : Model -> Html Msg
view { projects, focus, new } =
  div []
    [ viewNewForm new
    , List.map (viewItem focus) projects |> ul []
    ]
  
activeStyle : List (String, String)
activeStyle =
  [ ("color", "red") ]
      
viewItem : Maybe ProjectId -> Project -> Html Msg
viewItem focus { id, name } =
  let
    styles = focus
                |> Maybe.map (\i -> i == id)
                |> Maybe.withDefault False
                |> (\isActive -> if isActive then activeStyle else [])

  in
    li [ style styles]
      [ a [ href "#", onClick (Focus id) ]
          [ text name ]
      ]


viewNewForm : NewProject -> Html Msg
viewNewForm project =
  form []
    [ input [ type_ "text"
            ,  action "#"
            , placeholder "Name"
            , onInput Name
            , value project.name ]
        []
    , button [ onClick (Submit project) ]
        [ text "+" ]
      ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FetchAll -> model ! []
    Load projects ->
      { model | projects = projects } ! []

    Focus id ->
      model.projects
        |> List.filter (\p -> p.id == id)
        |> List.head
        |> Maybe.map .id
        |> (\f -> { model | focus = f })
        |> pure
    Name n -> 
      let
        prev = model.new
        new = { prev | name = n }
      in
        { model | new = new } ! []
    Desc d ->
      let
        prev = model.new
        new = { prev | description = d }
      in
        { model | new = new } ! []
    Submit p -> model ! []
    Add p ->
      let 
        prev = model.new
        new = { prev | name = "", description = "" }
        projects = model.projects
      in
        { model | projects = p :: projects, new = new } ! []

decodeProjects : Decoder (List Project)
decodeProjects = list decodeProject

decodeProject : Decoder Project
decodeProject =
  Project
    |$| field "id" string
    |*| field "name" string
    |*| field "description" (maybe string)
    |*| field "status" string

encodeNew : NewProject -> Encode.Value
encodeNew new =
  Encode.object
    [ ("name", Encode.string new.name) ]
