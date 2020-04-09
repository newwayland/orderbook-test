module Model.Random exposing (Seed, init, newSeed, resetSeed, step)

import Random



{- Hold the initial integer seed as well as the current random seed -}


type alias Seed =
    { display : Int
    , seed : Random.Seed
    }


init : Seed
init =
    newSeed defaultSeedValue



{- Create a new seed from a supplied integer -}


newSeed : Int -> Seed
newSeed displayValue =
    Seed displayValue (Random.initialSeed displayValue)



{- Create a new seed from the stored integer seed -}


resetSeed : Seed -> Seed
resetSeed currentSeed =
    { currentSeed | seed = Random.initialSeed currentSeed.display }



{- Run a generator with the current seed then update the seed
   with the output seed from the generator
-}


step : Random.Generator a -> Seed -> ( a, Seed )
step gen seed =
    let
        ( output, nextSeed ) =
            Random.step gen seed.seed
    in
    ( output, { seed | seed = nextSeed } )


defaultSeedValue : Int
defaultSeedValue =
    42
