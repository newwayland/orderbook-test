module Model.Random exposing (Seed, changeSeed, init, randomNameGenerator, scaledAgeGenerator, step)

import Array exposing (Array)
import Model.Names exposing (surnames)
import Random
import Random.Array
import Random.Int


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


step : Random.Generator a -> Seed -> ( a, Seed )
step gen seed =
    let
        ( output, newSeed ) =
            Random.step gen seed.seed
    in
    ( output, { seed | seed = newSeed } )


defaultSeedValue : Int
defaultSeedValue =
    42



{- Scales up a random positive integer to about 100 years in microseconds -}


scaledAgeGenerator : Random.Generator Int
scaledAgeGenerator =
    Random.map (\x -> x * 1470) Random.Int.positiveInt


randomNameGenerator : Array String -> Random.Generator String
randomNameGenerator firstnames =
    Random.map2 (\x y -> x ++ " " ++ y)
        (randomName firstnames)
        (randomName surnames)


randomName : Array String -> Random.Generator String
randomName =
    Random.Array.sample >> Random.map (Maybe.withDefault "Oops")
