module Main exposing (..)

import Html.App
import App
import TestDispatch

main : Program Never
main =
  Html.App.program
    { init = App.init
    , view = App.view
    , update = App.update TestDispatch.dispatch
    , subscriptions = App.subscriptions
    }
