module Update exposing (Msg(..), init, update)

import Model exposing (Model)
import Model.Clock exposing (advanceTime, decreaseSpeed, increaseSpeed, setTimeHere)
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
            ( { model | clock = Model.Clock.advanceTime model.clock }, Cmd.none )

        SetTimeHere localTime ->
            ( { model | clock = Model.Clock.setTimeHere localTime model.clock }, Cmd.none )

        IncreaseSpeed ->
            ( { model | clock = Model.Clock.increaseSpeed model.clock }, Cmd.none )

        DecreaseSpeed ->
            ( { model | clock = Model.Clock.decreaseSpeed model.clock }, Cmd.none )


requestLocalTime : Cmd Msg
requestLocalTime =
    Task.map2 Tuple.pair Time.here Time.now
        |> Task.perform SetTimeHere
