module Model.Random exposing (Seed, changeSeed, init, randomNameGenerator, scaledAgeGenerator, step)

import Array exposing (Array)
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


buildIntList : Int -> Random.Seed -> ( Array Int, Random.Seed )
buildIntList length seed =
    let
        arrayBuilder =
            Random.Array.array length Random.Int.positiveInt
    in
    Random.step arrayBuilder seed


defaultSeedValue : Int
defaultSeedValue =
    42



{- Scales up a random positive integer to about 100 years in microseconds -}


scaledAgeGenerator : Random.Generator Int
scaledAgeGenerator =
    Random.map (\x -> x * 1470) Random.Int.positiveInt


randomNameGenerator : Random.Generator String
randomNameGenerator =
    Random.map2 (\x y -> x ++ " " ++ y)
        (randomName firstnames)
        (randomName surnames)


randomName : Array String -> Random.Generator String
randomName selection =
    Random.Array.sample selection |> Random.map (Maybe.withDefault "Oops")


firstnames : Array String
firstnames =
    Array.fromList [ "Fred", "Jim" ]


surnames : Array String
surnames =
    Array.fromList [ "Bloggs", "Bo" ]
