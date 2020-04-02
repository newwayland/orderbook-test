module Model.Clock exposing
    ( Clock
    , init, setTimeHere, advanceTime, increaseSpeed
    , decreaseSpeed, pause, normalSpeed, fastSpeed, fullSpeed
    , paused, tickSpeed, toHour, toMinute, toSecond, age, toDisplayMonth
    , DateTime, toDateTime
    )

{-| The clock ADT for the simulationn


# Definition

@docs Clock


# Updaters

@docs init, setTimeHere, advanceTime, increaseSpeed
@docs decreaseSpeed, pause, normalSpeed, fastSpeed, fullSpeed


# Queries

@docs paused, tickSpeed, toHour, toMinute, toSecond, age, toDisplayMonth

-}

import Duration exposing (Duration)
import Quantity
import Time exposing (Month(..), Posix, Zone)
import Time.Extra exposing (Interval(..))



{- A representation of the current time in the simulation that can be
   speeded up and slowed down.
-}


type alias Clock =
    { zone : Zone
    , time : Posix
    , speed : Duration
    }



{- An set of Integers representing a date and a time -}


type alias DateTime =
    Time.Extra.Parts


init : Clock
init =
    Clock Time.utc (Time.millisToPosix 0) minSpeed |> pause



-- Exposed
{- Set the simulation time to the current real world local time. -}


setTimeHere : ( Zone, Posix ) -> Clock -> Clock
setTimeHere ( newZone, newTime ) clock =
    let
        startofDay =
            Time.Extra.floor Day newZone newTime
    in
    { clock | zone = newZone, time = startofDay }



{- Move the time forward by one simulated second. -}


advanceTime : Clock -> Clock
advanceTime clock =
    { clock | time = advancePeriod |> addDuration clock.time }



{- Halt the tick process in the simulation -}


pause : Clock -> Clock
pause clock =
    { clock | speed = Quantity.twice minSpeed }



{- Set the normal speed for the simulation -}


normalSpeed : Clock -> Clock
normalSpeed clock =
    { clock | speed = Duration.milliseconds 1024 }



{- Set the fast speed for the simulation -}


fastSpeed : Clock -> Clock
fastSpeed clock =
    { clock | speed = Duration.milliseconds 32 }



{- Set full speed for the simulation -}


fullSpeed : Clock -> Clock
fullSpeed clock =
    { clock | speed = maxSpeed }



{- Increase the delay between each tick in the simulation. -}


increaseSpeed : Clock -> Clock
increaseSpeed clock =
    if clock.speed |> Quantity.lessThan maxSpeed then
        clock

    else if clock.speed |> Quantity.greaterThan minSpeed then
        { clock | speed = minSpeed }

    else
        { clock | speed = Quantity.half clock.speed }



{- Decrease the delay between each tick in the simulation. -}


decreaseSpeed : Clock -> Clock
decreaseSpeed clock =
    if paused clock then
        clock

    else if clock.speed |> Quantity.lessThan maxSpeed then
        { clock | speed = maxSpeed }

    else
        { clock | speed = Quantity.twice clock.speed }



-- Queries
{- Is the simulation paused? -}


paused : Clock -> Bool
paused clock =
    clock.speed |> Quantity.greaterThan minSpeed



{- The current delay between each tick in the simulation in milliseconds. -}


tickSpeed : Clock -> Int
tickSpeed clock =
    durationInMs clock.speed



{- Return a DateTime from the supplied Clock -}


toDateTime : Clock -> DateTime
toDateTime clock =
    Time.Extra.posixToParts clock.zone clock.time



{- Translate the month type into a string -}


toDisplayMonth : Month -> String
toDisplayMonth month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"



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



{- Duration since birthdate in years -}


age : Clock -> Posix -> Int
age clock birthdate =
    Time.Extra.diff Year clock.zone birthdate clock.time



-- Helpers


addDuration : Posix -> Duration -> Posix
addDuration initialTime length =
    let
        timeInMs =
            Time.posixToMillis initialTime
    in
    Time.millisToPosix (timeInMs + durationInMs length)


durationInMs : Duration -> Int
durationInMs =
    Duration.inMilliseconds >> floor


minSpeed : Duration
minSpeed =
    Duration.milliseconds 4096


maxSpeed : Duration
maxSpeed =
    Duration.milliseconds 1


advancePeriod : Duration
advancePeriod =
    Duration.hours 8
