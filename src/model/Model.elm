module Model exposing (Model, init)

import Bootstrap.Accordion as Accordion
import Model.Clock exposing (Clock)
import Model.Individuals exposing (Individuals)
import Model.Random exposing (Seed)
import OrderBook exposing (OrderBook)


type alias Model =
    { clock : Clock
    , seed : Seed
    , individuals : Individuals
    , labour : OrderBook
    , products : OrderBook
    , accordionState : Accordion.State
    }


init : Model
init =
    Model
        Model.Clock.init
        Model.Random.init
        Model.Individuals.empty
        OrderBook.empty
        OrderBook.empty
        Accordion.initialState
