module Model exposing (..)

import Duration exposing (Duration)
import Model.Clock exposing (Clock)
import Quantity
import Time exposing (Posix, Zone)


type alias Model =
    { clock : Clock }


init : Model
init =
    Model Model.Clock.init
