module Update exposing (Msg(..), init, update)

import Model exposing (Model)
import Model.Clock
import Model.Random
import Random
import Task
import Time exposing (Posix, Zone)


type Msg
    = Tick Posix
    | SetTimeHere ( Zone, Posix )
    | IncreaseSpeed
    | DecreaseSpeed
    | Pause
    | NormalSpeed
    | FastSpeed
    | FullSpeed
    | ChangeSeed String
    | SetSeed Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model.init, requestLocalTime )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | clock = Model.Clock.advanceTime model.clock }, Cmd.none )

        SetTimeHere localTime ->
            ( { model | clock = Model.Clock.setTimeHere localTime model.clock }, requestNewSeed )

        IncreaseSpeed ->
            ( { model | clock = Model.Clock.increaseSpeed model.clock }, Cmd.none )

        DecreaseSpeed ->
            ( { model | clock = Model.Clock.decreaseSpeed model.clock }, Cmd.none )

        Pause ->
            ( { model | clock = Model.Clock.pause model.clock }, Cmd.none )

        NormalSpeed ->
            ( { model | clock = Model.Clock.normalSpeed model.clock }, Cmd.none )

        FastSpeed ->
            ( { model | clock = Model.Clock.fastSpeed model.clock }, Cmd.none )

        FullSpeed ->
            ( { model | clock = Model.Clock.fullSpeed model.clock }, Cmd.none )

        ChangeSeed inputString ->
            ( { model | seed = getSeedValue inputString |> Model.Random.changeSeed }, Cmd.none )

        SetSeed input ->
            ( { model | seed = Model.Random.changeSeed input }, Cmd.none )


requestLocalTime : Cmd Msg
requestLocalTime =
    Task.map2 Tuple.pair Time.here Time.now
        |> Task.perform SetTimeHere


requestNewSeed : Cmd Msg
requestNewSeed =
    Random.generate SetSeed randomInt


randomInt : Random.Generator Int
randomInt =
    Random.int 1 Random.maxInt


getSeedValue : String -> Int
getSeedValue seedValue =
    Maybe.withDefault 0 (String.toInt seedValue)
