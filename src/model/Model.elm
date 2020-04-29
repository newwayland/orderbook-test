module Model exposing (Model, init)

import Bootstrap.Accordion as Accordion
import Model.Clock exposing (Clock)
import Model.Cursor
import Model.Individuals exposing (Individuals)
import Model.Markets exposing (Markets)
import Model.Random exposing (Seed)


type alias Model =
    { clock : Clock
    , seed : Seed
    , individuals : Individuals
    , markets : Markets
    , accordionState : Accordion.State
    }


init : Model
init =
    Model
        Model.Clock.init
        Model.Random.init
        Model.Cursor.empty
        Model.Markets.empty
        Accordion.initialState
