module Model.Clock exposing
    ( Clock
    , advanceTime
    , decreaseSpeed
    , increaseSpeed
    , init
    , paused
    , setTimeHere
    , tickSpeed
    , toHour
    , toMinute
    , toSecond
    )

import Duration exposing (Duration)
import Quantity
import Time exposing (Posix, Zone)


type alias Clock =
    { zone : Zone
    , time : Posix
    , speed : Duration
    }


init : Clock
init =
    Clock Time.utc (Time.millisToPosix 0) (Duration.milliseconds 1024)



-- Exposed


setTimeHere : ( Zone, Posix ) -> Clock -> Clock
setTimeHere ( newZone, newTime ) clock =
    { clock | zone = newZone, time = newTime }


tickSpeed : Clock -> Int
tickSpeed clock =
    durationInMs clock.speed


advanceTime : Clock -> Clock
advanceTime clock =
    { clock | time = Duration.second |> addDuration clock.time }


increaseSpeed : Clock -> Clock
increaseSpeed clock =
    if clock.speed |> Quantity.lessThan minSpeed then
        clock

    else if clock.speed |> Quantity.greaterThan maxSpeed then
        { clock | speed = maxSpeed }

    else
        { clock | speed = Quantity.half clock.speed }


decreaseSpeed : Clock -> Clock
decreaseSpeed clock =
    if paused clock then
        clock

    else if clock.speed |> Quantity.lessThan minSpeed then
        { clock | speed = minSpeed }

    else
        { clock | speed = Quantity.twice clock.speed }



-- Queries


paused : Clock -> Bool
paused clock =
    clock.speed |> Quantity.greaterThan maxSpeed


toHour : Clock -> Int
toHour clock =
    Time.toHour clock.zone clock.time


toMinute : Clock -> Int
toMinute clock =
    Time.toMinute clock.zone clock.time


toSecond : Clock -> Int
toSecond clock =
    Time.toSecond clock.zone clock.time



-- Private


addDuration : Posix -> Duration -> Posix
addDuration initialTime length =
    let
        timeInMs =
            Time.posixToMillis initialTime
    in
    Time.millisToPosix (timeInMs + durationInMs length)


durationInMs : Duration -> Int
durationInMs dur =
    Duration.inMilliseconds dur |> floor


maxSpeed : Duration
maxSpeed =
    Duration.milliseconds 4096


minSpeed : Duration
minSpeed =
    Duration.milliseconds 1
