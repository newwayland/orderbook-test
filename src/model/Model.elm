module Model exposing (..)

import Duration exposing (Duration)
import Model.Clock exposing (Clock)
import Model.Random exposing (Seed)
import Quantity
import Time exposing (Posix, Zone)


type alias Model =
    { clock : Clock
    , seed : Seed
    }


init : Model
init =
    Model Model.Clock.init Model.Random.init
