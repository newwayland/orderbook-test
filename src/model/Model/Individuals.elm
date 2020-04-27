module Model.Individuals exposing
    ( Individuals, IndividualsArray
    , empty, initFromArray, indexedEmpty
    , moveCursor, incrementCursor, decrementCursor
    , foldlIndexed, map, push
    , current, index
    , defaultLength, length
    , atMin, atMax
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individuals, IndividualsArray, Sex


# Updaters

@docs empty, initFromArray, indexedEmpty
@docs moveCursor, incrementCursor, decrementCursor
@docs advanceTime, foldlIndexed, map, push


# Queries

@docs current, index
@docs defaultLength, length
@docs atMin, atMax

-}

import Array exposing (Array)
import Model.Individual exposing (Individual)



{- Indexed list of individuals -}


type alias IndividualsArray =
    Array Individual



{- Indexed list of individuals with a current individual cursor -}


type alias Individuals =
    { current : Int
    , individuals : IndividualsArray
    }


{-| An empty individual list
-}
empty : Individuals
empty =
    initFromArray Array.empty


{-| An empty list with the cursor set to the same value as an existing list
-}
indexedEmpty : Individuals -> Individuals
indexedEmpty old =
    Individuals old.current Array.empty


{-| Build a cursored individal from an indexed list of individual
-}
initFromArray : IndividualsArray -> Individuals
initFromArray =
    Individuals 0


{-| Change the current individual
-}
moveCursor : Individuals -> Int -> Individuals
moveCursor inds value =
    { inds | current = clamp (minIndex inds) (maxIndex inds) value }


{-| Decrease the cursor
-}
decrementCursor : Individuals -> Individuals
decrementCursor inds =
    moveCursor inds (inds.current - 1)


{-| Increase the cursor
-}
incrementCursor : Individuals -> Individuals
incrementCursor inds =
    moveCursor inds (inds.current + 1)


{-| Use the cursor value to extract the current individual from the set
of individuals supplied
-}
current : Individuals -> Individual
current individuals =
    case Array.get individuals.current individuals.individuals of
        Just value ->
            value

        Nothing ->
            Model.Individual.defaultIndividual


index : Individuals -> Int
index individuals =
    individuals.current


{-| Is the cursor at the minimum index?
-}
atMin : Individuals -> Bool
atMin inds =
    inds.current == minIndex inds


{-| Is the cursor at the maximum index?
-}
atMax : Individuals -> Bool
atMax inds =
    inds.current == maxIndex inds


{-| The length of the list of individuals
-}
length : Individuals -> Int
length inds =
    Array.length inds.individuals


{-| The index of the individual at the end of the list of individuals
-}
maxIndex : Individuals -> Int
maxIndex inds =
    length inds - 1


{-| The index of the individual at the beginning of the list of individuals
-}
minIndex : Individuals -> Int
minIndex _ =
    0


{-| The default length of the individuals list
-}
defaultLength : Int
defaultLength =
    1000


{-| Send the time tick to each individual in the list
-}
map : (Individual -> Individual) -> Individuals -> Individuals
map mapper inds =
    { inds | individuals = Array.map mapper inds.individuals }


{-| Push an individal onto the end of the individuals list
-}
push : Individual -> Individuals -> Individuals
push ind inds =
    { inds | individuals = Array.push ind inds.individuals }


{-| Create an indexed list of individuals. Each individual will be paired with its index
-}
foldlIndexed : (( Int, Individual ) -> b -> b) -> b -> Individuals -> b
foldlIndexed tagger acc inds =
    Array.toIndexedList inds.individuals |> List.foldl tagger acc
