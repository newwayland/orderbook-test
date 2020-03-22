module Model exposing (..)

import Duration exposing (Duration)
import Quantity
import Task
import Time exposing (Posix, Zone)


type alias Model =
    { zone : Zone
    , time : Posix
    , speed : Duration.Duration
    }


type Msg
    = Tick Posix
    | SetTimeHere ( Zone, Posix )
    | IncreaseSpeed
    | DecreaseSpeed



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) Duration.second
    , Task.map2 Tuple.pair Time.here Time.now
        |> Task.perform SetTimeHere
    )


addDuration : Posix -> Duration -> Posix
addDuration initialTime length =
    let
        timeInMs =
            Time.posixToMillis initialTime

        durationInMs =
            Duration.inMilliseconds length
    in
    Time.millisToPosix (timeInMs + floor durationInMs)


tickSpeed : Model -> Float
tickSpeed model =
    Duration.inMilliseconds model.speed


advanceTime : Model -> Model
advanceTime model =
    { model | time = Duration.second |> addDuration model.time }


increaseSpeed : Model -> Model
increaseSpeed model =
    { model | speed = Quantity.twice model.speed }


decreaseSpeed : Model -> Model
decreaseSpeed model =
    { model | speed = Quantity.half model.speed }
