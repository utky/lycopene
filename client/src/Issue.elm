module Issue exposing (..)

import Html exposing (Html, map, form, div, textarea, text, input, i)
import Html.Attributes exposing (class, type_, placeholder)
import Html.Events exposing (onInput)


type alias Issue =
  { id : String
  , title : String
  , description : Maybe String
  , status : String
  }

type alias NewIssue =
  { title : String
  , description : String
}

type alias Issues =
  { issues : List Issue
  , newIssue : NewIssue
  }

type Msg
  = UpdateNew NewMsg

type NewMsg
  = Title String
  | Description String

initNewIssue : NewIssue
initNewIssue =
  { title = ""
  , description = ""
  }

init : Issues
init =
  { issues = []
  , newIssue = initNewIssue
  }

view : Issues -> Html Msg
view model =
  div
    [ class "row" ]
    [ div 
        [ class "seven columns" ]
        [ i
            [ class "fa fa-plus" ]
            []
        ,  map UpdateNew (viewNewIssue model.newIssue)
        , viewIssues model.issues
        ]
    , div
        [ class "five columns" ]
        [ text "details" ]
    ]

viewNewIssue : NewIssue -> Html NewMsg
viewNewIssue newIssue =
  form
    []
    [ div
        [ class "row" ]
        [ input
            [ type_ "text"
            , class "twelve columns"
            , placeholder "Title"
            , onInput Title
            ]
            []
        ]
    , div
        [ class "row" ]
        [ textarea
            [ placeholder "Description"
            , onInput Description
            , class "twelve columns"
            ]
            []
        ]
    ]

viewIssues : List Issue -> Html Msg
viewIssues issues =
  let viewIssue issue = 
        div
          [ class "row" ]
          [ div
              [ class "twelve columns" ]
              [ text issue.title
              ]
          ]
  in  div
        [ class "container issues" ]
        (List.map viewIssue issues)
            

update : Msg -> Issues -> ( Issues, Cmd Msg )
update msg model =
  case msg of

    UpdateNew m ->
      let (n, c) = updateNew m model.newIssue
      in  ({ model | newIssue = n}, Cmd.map UpdateNew c)

updateNew : NewMsg -> NewIssue -> ( NewIssue, Cmd NewMsg )
updateNew msg model =
  case msg of

    Title title -> { model | title = title } ! []
    Description desc -> { model | description = desc } ! []
