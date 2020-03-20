module Main exposing (..)
-- Show the current time in your time zone.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/time.html
--
-- For an analog clock, check out this SVG example:
--   https://elm-lang.org/examples/clock
--

import Browser
import Html exposing (Html, button, div, text, h1)
import Html.Events exposing (onClick)
import Task exposing (Task)
import Time exposing (Posix)
import Duration exposing(Duration)
import Quantity 




-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

addDuration: Duration -> Posix -> Posix
addDuration length initialTime =
    let 
        timeInMs = Time.posixToMillis initialTime
        durationInMs = Duration.inMilliseconds length
    in
        Time.millisToPosix (timeInMs + floor durationInMs)

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , speed: Duration.Duration
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) Duration.second
  , Task.map2 Tuple.pair Time.here Time.now |> Task.perform SetTimeHere 
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | SetTimeHere (Time.Zone, Posix)
  | IncreaseSpeed 
  | DecreaseSpeed 



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = addDuration Duration.second model.time}
      , Cmd.none
      )

    SetTimeHere (newZone,newTime)->
      ( { model | zone = newZone, time = newTime }
      , Cmd.none
      )

    IncreaseSpeed ->
       ( { model | speed = Quantity.plus (Duration.seconds 0.5) model.speed }
       , Cmd.none
       )

    DecreaseSpeed ->
        ( { model | speed = Quantity.minus (Duration.seconds 0.5) model.speed }
        , Cmd.none
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Duration.inMilliseconds model.speed) Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    hour   = Time.toHour model.zone model.time
            |> String.fromInt 
    minute = Time.toMinute model.zone model.time
            |> String.fromInt
            |> String.padLeft 2 '0'
    second = Time.toSecond model.zone model.time
            |> String.fromInt
            |> String.padLeft 2 '0'
  in
  div []
  [
    h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
    , div []
    [ button [onClick DecreaseSpeed] [text "-"]
    , div[] [ text (Duration.inSeconds model.speed |> String.fromFloat)]
    , button [onClick IncreaseSpeed] [text "+"]
    ]
  ]
