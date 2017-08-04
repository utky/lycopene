module Test exposing (..)

import Html exposing (program)
import App
import TestDispatch

main : Program Never App.State App.Msg
main =
  program
    { init = App.init
    , view = App.view
    , update = App.update TestDispatch.perform
    , subscriptions = App.subscriptions
    }
