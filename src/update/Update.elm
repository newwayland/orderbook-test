module Update exposing (Msg(..), init, update)

import Model exposing (Model)
import Model.Clock
import Model.Individual
import Model.Random
import Model.RandomNames
import Random
import Random.Int
import Task
import Time exposing (Posix, Zone)


type Msg
    = Tick Posix
      -- Clock Messages
    | ResetModelFromTime ( Zone, Posix )
    | IncreaseSpeed
    | DecreaseSpeed
    | Pause
    | NormalSpeed
    | FastSpeed
    | FullSpeed
      -- Model Seed Messages
    | ChangeSeed String
    | UpdateSeedFrom Int
    | ResetModel
    | ResetSeed
      -- Individual Cursor Messages
    | ChangeCursor String
    | UpdateCursorFrom Int
    | RandomCursor
    | DecrementCursor
    | IncrementCursor


{-| Create the model and start the initialisation message sequence
-}
init : () -> ( Model, Cmd Msg )
init _ =
    ( Model.init, Random.Int.positiveInt |> Random.generate UpdateSeedFrom )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- INITIALISATION SEQUENCE
        UpdateSeedFrom initialSeed ->
            ( { model | seed = Model.Random.newSeed initialSeed }, requestLocalTime ResetModelFromTime )

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
            ( { model | seed = getValue inputString |> Model.Random.newSeed }, Cmd.none )

        ChangeCursor inputString ->
            ( { model | individuals = getValue inputString |> Model.Individual.moveCursor model.individuals }, Cmd.none )

        UpdateCursorFrom newCursor ->
            ( { model | individuals = Model.Individual.moveCursor model.individuals newCursor }, Cmd.none )

        DecrementCursor ->
            ( { model | individuals = Model.Individual.decrementCursor model.individuals }, Cmd.none )

        IncrementCursor ->
            ( { model | individuals = Model.Individual.incrementCursor model.individuals }, Cmd.none )

        RandomCursor ->
            ( model, Model.RandomNames.randomIndividualIndex model.individuals |> Random.generate UpdateCursorFrom )

        ResetModel ->
            ( { model | seed = Model.Random.resetSeed model.seed }, requestLocalTime ResetModelFromTime )

        ResetSeed ->
            init ()

        ResetModelFromTime localTime ->
            ( { model | clock = Model.Clock.setTimeHere localTime model.clock } |> initSeededItemsInModel, Cmd.none )


{-| Run a set of tasks to obtain the current time and time zone and
send them to the supplied message
-}
requestLocalTime : (( Zone, Posix ) -> Msg) -> Cmd Msg
requestLocalTime msg =
    Task.map2 Tuple.pair Time.here Time.now
        |> Task.perform msg


{-| Convert a string seed to a number with a default of zero
-}
getValue : String -> Int
getValue value =
    Maybe.withDefault 0 (String.toInt value)


{-| Update attributes in Model that depend upon the Random seed
-}
initSeededItemsInModel : Model -> Model
initSeededItemsInModel model =
    let
        individualsGenerator =
            Model.Clock.calculateBirthDate model.clock |> Model.RandomNames.randomIndividuals

        ( individualArray, nextSeed ) =
            Model.Random.step individualsGenerator model.seed
    in
    { model | seed = nextSeed, individuals = Model.RandomNames.initFromArray individualArray }
