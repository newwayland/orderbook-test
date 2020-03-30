module Model.Individual exposing
    ( Individual, Individuals
    , current
    , init
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individual, Individuals


# Updaters


# Queries

@docs current

-}

import Array exposing (Array)



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
    , age : Int
    }


init : Individuals
init =
    Individuals 1 (Array.repeat 3 createIndividual)


createIndividual : Individual
createIndividual =
    Individual "Fred" 42


{-| If we see this bad things have happened!
-}
oops : Individual
oops =
    Individual "Oops" 0


{-| Use the cursor value to extract the current individual from the set
of individuals supplied
-}
current : Individuals -> Individual
current individuals =
    case Array.get individuals.current individuals.individuals of
        Just value ->
            value

        Nothing ->
            oops
