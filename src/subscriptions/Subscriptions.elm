module Subscriptions exposing (subscriptions)

import Model exposing (Model)
import Time
import Update exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    if Model.paused model then
        Sub.none

    else
        Time.every (tickSpeedAsFloat model) Tick


tickSpeedAsFloat : Model -> Float
tickSpeedAsFloat model =
    Model.tickSpeed model |> toFloat
