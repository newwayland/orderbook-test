module Update exposing (Msg(..), init, update)

import Bootstrap.Accordion as Accordion
import Model exposing (Model)
import Model.Clock
import Model.Cursor
import Model.Individuals
import Model.Polity
import Model.Random
import Model.RandomNames
import Model.TaskManager
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
      -- Polity Population Messages
    | ChangePopulation String
      -- Polity Age Category Messages
    | ChangeSchoolAge String
    | ChangeWorkingAge String
    | ChangeRetirementAge String
      -- Individual Cursor Messages
    | ChangeIndividualCursor String
    | UpdateIndividualCursorFrom Int
    | RandomIndividualCursor
    | DecrementIndividualCursor
    | IncrementIndividualCursor
      -- Market Cursor Messages
    | DecrementMarketCursor
    | IncrementMarketCursor
      -- Accordion Messages
    | AccordionMsg Accordion.State


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
            ( advanceTime model, Cmd.none )

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

        ChangePopulation inputString ->
            ( { model | polity = getValue inputString |> Model.Polity.changePopulation model.polity }, Cmd.none )

        ChangeSeed inputString ->
            ( { model | seed = getValue inputString |> Model.Random.newSeed }, Cmd.none )

        ChangeIndividualCursor inputString ->
            ( { model | individuals = getValue inputString |> Model.Cursor.moveCursor model.individuals }, Cmd.none )

        UpdateIndividualCursorFrom newCursor ->
            ( { model | individuals = Model.Cursor.moveCursor model.individuals newCursor }, Cmd.none )

        DecrementIndividualCursor ->
            ( { model | individuals = Model.Cursor.decrementCursor model.individuals }, Cmd.none )

        IncrementIndividualCursor ->
            ( { model | individuals = Model.Cursor.incrementCursor model.individuals }, Cmd.none )

        RandomIndividualCursor ->
            ( model, Model.RandomNames.randomIndividualIndex model.individuals |> Random.generate UpdateIndividualCursorFrom )

        DecrementMarketCursor ->
            ( { model | markets = Model.Cursor.decrementCursor model.markets }, Cmd.none )

        IncrementMarketCursor ->
            ( { model | markets = Model.Cursor.incrementCursor model.markets }, Cmd.none )

        ChangeSchoolAge newAge ->
            ( { model | polity = Model.Polity.changeSchoolAge (String.toInt newAge) model.polity }, Cmd.none )

        ChangeWorkingAge newAge ->
            ( { model | polity = Model.Polity.changeMajority (String.toInt newAge) model.polity }, Cmd.none )

        ChangeRetirementAge newAge ->
            ( { model | polity = Model.Polity.changeRetirementAge (String.toInt newAge) model.polity }, Cmd.none )

        ResetModel ->
            ( Model.reset model, requestLocalTime ResetModelFromTime )

        ResetSeed ->
            init ()

        ResetModelFromTime localTime ->
            ( { model | clock = Model.Clock.setTimeHere localTime model.clock } |> initSeededItemsInModel, Cmd.none )

        AccordionMsg state ->
            ( { model | accordionState = state }, Cmd.none )


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
            Model.Clock.calculateBirthDate model.clock |> Model.RandomNames.randomIndividuals model.polity.population

        ( individualArray, nextSeed ) =
            Model.Random.step individualsGenerator model.seed
    in
    { model
        | seed = nextSeed
        , individuals =
            Model.RandomNames.initFromArray individualArray
                |> Model.Individuals.reindex
                |> Model.Cursor.retainIndex model.individuals
    }


{-| Run the main model update process for this time tick
-}
advanceTime : Model -> Model
advanceTime model =
    let
        advanceClock mod =
            { mod | clock = Model.Clock.advanceTime mod.clock }
    in
    Model.TaskManager.advanceTime model |> advanceClock
