module Model.RandomNames exposing (initFromArray, randomIndividualIndex, randomIndividuals)

import Model.Cursor
import Model.Individual exposing (Sex(..))
import Model.Individuals exposing (Individuals, IndividualsArray)
import Model.Names exposing (NameArray)
import Model.Types exposing (BirthDate)
import Random exposing (Generator)
import Random.Array
import Random.Extra
import Random.Int



{- Build an individuals structure -}


initFromArray : IndividualsArray -> Individuals
initFromArray arr =
    Model.Cursor.initFromArray arr



{- Generate an IndividualsArray with random names and random ages -}


randomIndividuals : (Int -> BirthDate) -> Generator IndividualsArray
randomIndividuals birthDateCalculator =
    let
        randomIndividual =
            Random.map2
                (\( x, y ) z -> Model.Individual.newIndividual 0 x y z)
                nameSexPairGenerator
                (randomBirthDateGenerator birthDateCalculator)
    in
    Random.Array.array Model.Individuals.defaultLength randomIndividual



{- Generate a tuple of a name and associated sex marker, 50/50 Male/Female -}


nameSexPairGenerator : Generator ( String, Sex )
nameSexPairGenerator =
    Random.Extra.choice Male Female
        |> Random.andThen
            (\sex ->
                Random.map (\x -> ( x, sex )) (selectFirstNameBySex sex |> randomNameGenerator)
            )



{- Pick a list of first names based upon Sex -}


selectFirstNameBySex : Sex -> NameArray
selectFirstNameBySex sex =
    case sex of
        Male ->
            Model.Names.maleFirst

        Female ->
            Model.Names.femaleFirst



{- Scales up a random positive integer to about 100 years worth of microseconds -}


scaledAgeGenerator : Random.Generator Int
scaledAgeGenerator =
    Random.map (\x -> x * 1470) Random.Int.positiveInt



{- Generate a random full name using a supplied list of first names -}


randomNameGenerator : NameArray -> Random.Generator String
randomNameGenerator firstnames =
    Random.map2 (\x y -> x ++ " " ++ y)
        (selectNameAtRandom firstnames)
        (selectNameAtRandom Model.Names.surnames)



{- generate a random name with a default -}


selectNameAtRandom : NameArray -> Random.Generator String
selectNameAtRandom =
    Random.Array.sample >> Random.map (Maybe.withDefault Model.Individual.defaultName)



{- generate a random birthdate -}


randomBirthDateGenerator : (Int -> BirthDate) -> Generator BirthDate
randomBirthDateGenerator calc =
    Random.map calc scaledAgeGenerator



{- generate the index of a random Individual from the size of the indviduals list -}


randomIndividualIndex : Individuals -> Generator Int
randomIndividualIndex inds =
    let
        length =
            Model.Cursor.length inds
    in
    Random.int 0 (length - 1)
