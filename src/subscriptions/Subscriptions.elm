module Subscriptions exposing (subscriptions)

import Model exposing (Model)
import Model.Clock
import Time
import Update exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    if Model.Clock.paused model.clock then
        Sub.none

    else
        Time.every (tickSpeedAsFloat model) Tick


tickSpeedAsFloat : Model -> Float
tickSpeedAsFloat model =
    Model.Clock.tickSpeed model.clock |> toFloat
