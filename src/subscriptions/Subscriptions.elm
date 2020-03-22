module Subscriptions exposing (subscriptions)

import Model exposing (Model, Msg(..))
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Model.tickSpeed model) Tick
