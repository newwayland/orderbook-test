module Model exposing (Model, init)

import Bootstrap.Accordion as Accordion
import Model.Clock exposing (Clock)
import Model.Individuals exposing (Individuals)
import Model.Random exposing (Seed)


type alias Model =
    { clock : Clock
    , seed : Seed
    , individuals : Individuals
    , accordionState : Accordion.State
    }


init : Model
init =
    Model Model.Clock.init Model.Random.init Model.Individuals.init Accordion.initialState
