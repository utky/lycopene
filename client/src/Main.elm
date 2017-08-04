module Main exposing (..)

import Html exposing (program)
import App
import Dispatch

main : Program Never App.State App.Msg
main =
  program
    { init = App.init
    , view = App.view
    , update = App.update Dispatch.perform
    , subscriptions = App.subscriptions
    }
