module Model exposing (..)

import Duration exposing (Duration)
import Model.Clock exposing (Clock)
import Model.Individual exposing (Individuals)
import Model.Random exposing (Seed)
import Quantity
import Time exposing (Posix, Zone)


type alias Model =
    { clock : Clock
    , seed : Seed
    , individuals : Individuals
    }


init : Model
init =
    Model Model.Clock.init Model.Random.init Model.Individual.init
