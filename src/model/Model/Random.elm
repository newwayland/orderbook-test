module Model.Random exposing (Seed, changeSeed, init, integerList)

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


integerList : Int -> Seed -> ( Array Int, Seed )
integerList length seed =
    let
        ( intList, newSeed ) =
            buildIntList length seed.seed
    in
    ( intList, { seed | seed = newSeed } )


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
