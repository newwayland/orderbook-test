module Model.Clock exposing
    ( Clock
    , TimeOfDay(..)
    , init, setTimeHere, advanceTime, increaseSpeed
    , decreaseSpeed, pause, normalSpeed, fastSpeed, fullSpeed
    , paused, tickSpeed, toHour, toMinute, toSecond
    , age, calculateBirthDate
    , yearIntToInt
    , posixToDateTime, toDateTime
    , posixToTimeOfDay, toTimeOfDay
    , subscriptions
    , DateTime
    )

{-| The clock ADT for the simulationn


# Definition

@docs Clock
@docs TimeOfDay


# Updaters

@docs init, setTimeHere, advanceTime, increaseSpeed
@docs decreaseSpeed, pause, normalSpeed, fastSpeed, fullSpeed


# Queries

@docs paused, tickSpeed, toHour, toMinute, toSecond
@docs age, calculateBirthDate
@docs yearIntToInt
@docs posixToDateTime, toDateTime
@docs posixToTimeOfDay, toTimeOfDay


# Subscriptions

@docs subscriptions

-}

import Duration exposing (Duration)
import Model.Types exposing (YearInt(..))
import Quantity
import Time exposing (Month(..), Posix, Zone)
import Time.Extra exposing (Interval(..))



{- A representation of the current time in the simulation that can be
   speeded up and slowed down.
-}


type Clock
    = Clock
        { zone : Zone
        , time : Posix
        , speed : Duration
        }



{- Wherabouts in the day we are -}


type TimeOfDay
    = Morning
    | Midday
    | Evening



{- An set of Integers representing a date and a time -}


type alias DateTime =
    Time.Extra.Parts


init : Clock
init =
    Clock { zone = defaultZone, time = defaultTime, speed = defaultSpeed } |> pause



-- Exposed
{- Set the simulation time to the current real world local day - starting at 4am. -}


setTimeHere : ( Zone, Posix ) -> Clock -> Clock
setTimeHere ( newZone, newTime ) (Clock clock) =
    let
        startofDay =
            Time.Extra.floor Day newZone newTime
                |> Time.Extra.add Hour 4 newZone
    in
    Clock { clock | zone = newZone, time = startofDay }



{- Move the time forward by one period. -}


advanceTime : Clock -> Clock
advanceTime (Clock clock) =
    Clock { clock | time = advancePeriod |> addDuration clock.time }



{- Halt the tick process in the simulation -}


pause : Clock -> Clock
pause (Clock clock) =
    Clock { clock | speed = Quantity.twice minSpeed }



{- Set the normal speed for the simulation -}


normalSpeed : Clock -> Clock
normalSpeed (Clock clock) =
    Clock { clock | speed = Duration.milliseconds 1024 }



{- Set the fast speed for the simulation -}


fastSpeed : Clock -> Clock
fastSpeed (Clock clock) =
    Clock { clock | speed = Duration.milliseconds 32 }



{- Set full speed for the simulation -}


fullSpeed : Clock -> Clock
fullSpeed (Clock clock) =
    Clock { clock | speed = maxSpeed }



{- Increase the delay between each tick in the simulation. -}


increaseSpeed : Clock -> Clock
increaseSpeed (Clock clock) =
    if clock.speed |> Quantity.lessThan maxSpeed then
        Clock clock

    else if clock.speed |> Quantity.greaterThan minSpeed then
        Clock { clock | speed = minSpeed }

    else
        Clock { clock | speed = Quantity.half clock.speed }



{- Decrease the delay between each tick in the simulation. -}


decreaseSpeed : Clock -> Clock
decreaseSpeed (Clock clock) =
    if paused (Clock clock) then
        Clock clock

    else if clock.speed |> Quantity.lessThan maxSpeed then
        Clock { clock | speed = maxSpeed }

    else
        Clock { clock | speed = Quantity.twice clock.speed }



-- Queries
{- Is the simulation paused? -}


paused : Clock -> Bool
paused (Clock clock) =
    clock.speed |> Quantity.greaterThan minSpeed



{- The current delay between each tick in the simulation in milliseconds. -}


tickSpeed : Clock -> Int
tickSpeed (Clock clock) =
    durationInMs clock.speed



{- Return a DateTime from the current time -}


toDateTime : Clock -> DateTime
toDateTime (Clock clock) =
    posixToDateTime (Clock clock) clock.time



{- Return a TimeOfDay from the current time -}


toTimeOfDay : Clock -> TimeOfDay
toTimeOfDay (Clock clock) =
    posixToTimeOfDay (Clock clock) clock.time



{- Return a DateTime from a Posix relative to the current zone -}


posixToDateTime : Clock -> Posix -> DateTime
posixToDateTime (Clock clock) =
    Time.Extra.posixToParts clock.zone



{- Return a TimeOfDay from a Posix relative to the current zone -}


posixToTimeOfDay : Clock -> Posix -> TimeOfDay
posixToTimeOfDay clock time =
    let
        hour =
            (posixToDateTime clock time).hour
    in
    if hour <= 4 then
        Morning

    else if hour <= 12 then
        Midday

    else
        Evening



{- What hour is it (From 0 to 23) -}


toHour : Clock -> Int
toHour (Clock clock) =
    Time.toHour clock.zone clock.time



{- What minute is it (From 0 to 59) -}


toMinute : Clock -> Int
toMinute (Clock clock) =
    Time.toMinute clock.zone clock.time



{- What second is it (From 0 to 59) -}


toSecond : Clock -> Int
toSecond (Clock clock) =
    Time.toSecond clock.zone clock.time



{- Duration since birthdate in years -}


age : Clock -> Posix -> YearInt
age (Clock clock) birthdate =
    Time.Extra.diff Year clock.zone birthdate clock.time |> YearInt



{- calculate the birthdate by subtracting a given number of
   milliseconds from the clock time
-}


calculateBirthDate : Clock -> Int -> Posix
calculateBirthDate (Clock clock) timeInMs =
    Time.millisToPosix (Time.posixToMillis clock.time - timeInMs)



{- Convert a YearInt to a standard Int -}


yearIntToInt : YearInt -> Int
yearIntToInt (YearInt year) =
    year



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


defaultZone : Zone
defaultZone =
    Time.utc


defaultTime : Posix
defaultTime =
    Time.millisToPosix 0


defaultSpeed : Duration
defaultSpeed =
    minSpeed


subscriptions : Clock -> (Posix -> msg) -> Sub msg
subscriptions clock toMsg =
    if paused clock then
        Sub.none

    else
        Time.every (tickSpeed clock |> toFloat) toMsg
