module Model.Individual exposing
    ( Individual, Individuals
    , current
    , createIndividuals, defaultLength, init
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individual, Individuals


# Updaters


# Queries

@docs current

-}

import Array exposing (Array)
import Model.Types exposing (BirthDate)
import Time



{- A list of individuals with a pointer to the current individual
   we're interested in
-}


type alias Individuals =
    { current : Int
    , individuals : Array Individual
    }



{- a simplified model of an Individual going about their day -}


type alias Individual =
    { name : String
    , birthdate : BirthDate
    }


init : Individuals
init =
    Individuals 0 Array.empty


{-| Generate a set for Individuals from a list of random numbers
-}
createIndividuals : (Int -> Time.Posix) -> List Int -> Individuals
createIndividuals calculateBirthDate =
    Array.fromList >> Array.map (createIndividual calculateBirthDate) >> Individuals 1


createIndividual : (Int -> Time.Posix) -> Int -> Individual
createIndividual calculateBirthDate random =
    Individual "test" (calculateBirthDate random)


{-| Use the cursor value to extract the current individual from the set
of individuals supplied
-}
current : Individuals -> Individual
current individuals =
    case Array.get individuals.current individuals.individuals of
        Just value ->
            value

        Nothing ->
            defaultIndividual


defaultIndividual : Individual
defaultIndividual =
    Individual "Ooops Ghost" Model.Types.defaultBirthdate


defaultLength : Int
defaultLength =
    3
