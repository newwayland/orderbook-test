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


{-| Create the model and start the initialisation message sequence
-}
init : () -> ( Model, Cmd Msg )
init _ =
    ( Model.init, requestLocalTime SetTimeHere )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- INITIALISATION SEQUENCE
        SetTimeHere localTime ->
            ( { model | clock = Model.Clock.setTimeHere localTime model.clock }, requestNewSeed SetSeed )

        SetSeed newSeed ->
            ( { model | seed = Model.Random.changeSeed newSeed }, Cmd.none )

        -- OPERATIONAL
        Tick _ ->
            ( { model | clock = Model.Clock.advanceTime model.clock }, Cmd.none )

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


{-| Run a set of tasks to obtain the current time and time zone and
send them to the supplied message
-}
requestLocalTime : (( Zone, Posix ) -> Msg) -> Cmd Msg
requestLocalTime msg =
    Task.map2 Tuple.pair Time.here Time.now
        |> Task.perform msg


{-| Run a task to generate a random positive integer and send it to the
supplied message
-}
requestNewSeed : (Int -> Msg) -> Cmd Msg
requestNewSeed msg =
    Random.generate msg randomInt


{-| A Random generator that creates a positive integer
-}
randomInt : Random.Generator Int
randomInt =
    Random.int 1 Random.maxInt


{-| Convert a string seed to a number with a default of zero
-}
getSeedValue : String -> Int
getSeedValue seedValue =
    Maybe.withDefault 0 (String.toInt seedValue)
