module Model exposing (Model, init)

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
