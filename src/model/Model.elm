module Model exposing (Model, init, reset)

import Bootstrap.Accordion as Accordion
import Model.Clock exposing (Clock)
import Model.Cursor
import Model.Individuals exposing (Individuals)
import Model.Markets exposing (Markets)
import Model.Polity exposing (Polity)
import Model.Random exposing (Seed)


type alias Model =
    { clock : Clock
    , seed : Seed
    , individuals : Individuals
    , markets : Markets
    , polity : Polity
    , accordionState : Accordion.State
    }


init : Model
init =
    { clock = Model.Clock.init
    , seed = Model.Random.init
    , individuals = Model.Cursor.empty
    , markets = Model.Markets.empty
    , polity = Model.Polity.default
    , accordionState = Accordion.initialState
    }


reset : Model -> Model
reset model =
    { clock = Model.Clock.init
    , seed = Model.Random.resetSeed model.seed
    , individuals = Model.Cursor.empty |> Model.Cursor.retainIndex model.individuals
    , markets = Model.Markets.empty |> Model.Cursor.retainIndex model.markets
    , polity = Model.Polity.default
    , accordionState = model.accordionState
    }
