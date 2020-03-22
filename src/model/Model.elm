module Model exposing (..)

import Duration exposing (Duration)
import Quantity
import Time exposing (Posix, Zone)


type alias Model =
    { zone : Zone
    , time : Posix
    , speed : Duration
    }


init : Model
init =
    Model Time.utc (Time.millisToPosix 0) (Duration.milliseconds 1024)


addDuration : Posix -> Duration -> Posix
addDuration initialTime length =
    let
        timeInMs =
            Time.posixToMillis initialTime

        durationInMs =
            Duration.inMilliseconds length
    in
    Time.millisToPosix (timeInMs + floor durationInMs)


tickSpeed : Model -> Int
tickSpeed model =
    Duration.inMilliseconds model.speed |> floor


advanceTime : Model -> Model
advanceTime model =
    { model | time = Duration.second |> addDuration model.time }


increaseSpeed : Model -> Model
increaseSpeed model =
    if model.speed |> Quantity.lessThan minSpeed then
        model

    else if model.speed |> Quantity.greaterThan maxSpeed then
        { model | speed = maxSpeed }

    else
        { model | speed = Quantity.half model.speed }


decreaseSpeed : Model -> Model
decreaseSpeed model =
    if paused model then
        model

    else if model.speed |> Quantity.lessThan minSpeed then
        { model | speed = minSpeed }

    else
        { model | speed = Quantity.twice model.speed }


maxSpeed : Duration
maxSpeed =
    Duration.milliseconds 4096


minSpeed : Duration
minSpeed =
    Duration.milliseconds 1


paused : Model -> Bool
paused model =
    model.speed |> Quantity.greaterThan maxSpeed
