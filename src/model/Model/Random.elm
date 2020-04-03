module Model.Random exposing (Seed, changeSeed, init)

import Random


type alias Seed =
    { display : Int
    , seed : Random.Seed
    }


init : Seed
init =
    changeSeed defaultSeedValue


changeSeed : Int -> Seed
changeSeed displayValue =
    Seed displayValue (Random.initialSeed displayValue)


defaultSeedValue : Int
defaultSeedValue =
    42
