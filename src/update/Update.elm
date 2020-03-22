module Update exposing (Msg(..), init, update)

import Model exposing (Model, advanceTime, decreaseSpeed, increaseSpeed)
import Task
import Time exposing (Posix, Zone)


type Msg
    = Tick Posix
    | SetTimeHere ( Zone, Posix )
    | IncreaseSpeed
    | DecreaseSpeed


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model.init, requestLocalTime )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( Model.advanceTime model, Cmd.none )

        SetTimeHere ( newZone, newTime ) ->
            ( { model | zone = newZone, time = newTime }
            , Cmd.none
            )

        IncreaseSpeed ->
            ( Model.increaseSpeed model, Cmd.none )

        DecreaseSpeed ->
            ( Model.decreaseSpeed model, Cmd.none )


requestLocalTime : Cmd Msg
requestLocalTime =
    Task.map2 Tuple.pair Time.here Time.now
        |> Task.perform SetTimeHere
