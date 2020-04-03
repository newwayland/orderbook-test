module Model.Random exposing (Seed, changeSeed, init, integerList, randomInt)

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


integerList : Int -> Seed -> ( List Int, Seed )
integerList length seed =
    let
        ( intList, newSeed ) =
            buildIntList length seed.seed
    in
    ( intList, { seed | seed = newSeed } )


buildIntList : Int -> Random.Seed -> ( List Int, Random.Seed )
buildIntList currentLength seed =
    case currentLength of
        1 ->
            let
                ( newInt, newSeed ) =
                    Random.step randomInt seed
            in
            ( [ newInt ], newSeed )

        _ ->
            let
                ( newList, seed0 ) =
                    buildIntList (currentLength - 1) seed

                ( newInt, newSeed ) =
                    Random.step randomInt seed0
            in
            ( newInt :: newList, newSeed )


defaultSeedValue : Int
defaultSeedValue =
    42


{-| A Random generator that creates a positive integer
-}
randomInt : Random.Generator Int
randomInt =
    Random.int 1 Random.maxInt
