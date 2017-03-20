module Main exposing (..)

import Html exposing (program)
import App
import TestDispatch

main : Program Never App.AppState App.Msg
main =
  program
    { init = App.init
    , view = App.view
    , update = App.update TestDispatch.dispatch
    , subscriptions = App.subscriptions
    }
