module Internal exposing (promote, pure)

import Task

promote : msg -> Cmd msg
promote m = Task.perform (always m) (Task.succeed m)

pure : model -> ( model, Cmd msg )
pure model = model ! []
