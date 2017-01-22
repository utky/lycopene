module Style exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

container : Attribute msg
container = class "container"

custom : Html msg
custom =
  node "link"
    [ attribute "rel" "stylesheet"
    , attribute "type" "text/css"
    , attribute "href" "/assets/stylesheets/main.css" ] []


fonts : Html msg
fonts = 
  node "link"
    [ attribute "rel" "stylesheet"
    , attribute "href" "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic" ] []


skeleton : Html msg
skeleton =
  node "link"
    [ attribute "rel" "stylesheet"
    , attribute "type" "text/css"
    , attribute "href" "/assets/stylesheets/skeleton.css" ] []

normalize : Html msg
normalize =
  node "link"
    [ attribute "rel" "stylesheet"
    , attribute "type" "text/css"
    , attribute "href" "/assets/stylesheets/normalize.css" ] []

fontAwesome : Html msg
fontAwesome =
  node "link"
    [ attribute "rel" "stylesheet"
    , attribute "type" "text/css"
    , attribute "href" "/assets/stylesheets/font-awesome.min.css" ] []
