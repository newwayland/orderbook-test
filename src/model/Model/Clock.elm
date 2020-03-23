module Model.Clock exposing
    ( Clock
    , init, setTimeHere, advanceTime, increaseSpeed, decreaseSpeed
    , paused, tickSpeed, toHour, toMinute, toSecond
    )

{-| The clock ADT for the simulationn


# Definition

@docs Clock


# Updaters

@docs init, setTimeHere, advanceTime, increaseSpeed, decreaseSpeed


# Queries

@docs paused, tickSpeed, toHour, toMinute, toSecond

-}

import Duration exposing (Duration)
import Quantity
import Time exposing (Posix, Zone)



{- A representation of the current time in the simulation that can be
   speeded up and slowed down.
-}


type alias Clock =
    { zone : Zone
    , time : Posix
    , speed : Duration
    }


init : Clock
init =
    Clock Time.utc (Time.millisToPosix 0) (Duration.milliseconds 1024)



-- Exposed
{- Set the simulation time to the current real world local time. -}


setTimeHere : ( Zone, Posix ) -> Clock -> Clock
setTimeHere ( newZone, newTime ) clock =
    { clock | zone = newZone, time = newTime }



{- Move the time forward by one simulated second. -}


advanceTime : Clock -> Clock
advanceTime clock =
    { clock | time = Duration.second |> addDuration clock.time }



{- Increase the delay between each tick in the simulation. -}


increaseSpeed : Clock -> Clock
increaseSpeed clock =
    if clock.speed |> Quantity.lessThan minSpeed then
        clock

    else if clock.speed |> Quantity.greaterThan maxSpeed then
        { clock | speed = maxSpeed }

    else
        { clock | speed = Quantity.half clock.speed }



{- Decrease the delay between each tick in the simulation. -}


decreaseSpeed : Clock -> Clock
decreaseSpeed clock =
    if paused clock then
        clock

    else if clock.speed |> Quantity.lessThan minSpeed then
        { clock | speed = minSpeed }

    else
        { clock | speed = Quantity.twice clock.speed }



-- Queries
{- Is the simulation paused? -}


paused : Clock -> Bool
paused clock =
    clock.speed |> Quantity.greaterThan maxSpeed



{- The current delay between each tick in the simulation in milliseconds. -}


tickSpeed : Clock -> Int
tickSpeed clock =
    durationInMs clock.speed



{- What hour is it (From 0 to 23) -}


toHour : Clock -> Int
toHour clock =
    Time.toHour clock.zone clock.time



{- What minute is it (From 0 to 59) -}


toMinute : Clock -> Int
toMinute clock =
    Time.toMinute clock.zone clock.time



{- What second is it (From 0 to 59) -}


toSecond : Clock -> Int
toSecond clock =
    Time.toSecond clock.zone clock.time



-- Helpers


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
