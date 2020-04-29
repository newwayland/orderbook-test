module Model.Individuals exposing
    ( Individuals, IndividualsArray
    , reindex
    , defaultLength
    )

{-| A representation of an individual and what they do during the day


# Definition

@docs Individuals, IndividualsArray


# Updaters

@docs reindex


# Queries

@docs defaultLength

-}

import Array exposing (Array)
import Model.Individual exposing (Individual)



{- Indexed list of individuals -}


type alias IndividualsArray =
    Array Individual



{- Indexed list of individuals with a current individual cursor -}


type alias Individuals =
    { current : Int
    , content : IndividualsArray
    }


{-| The default length of the individuals list
-}
defaultLength : Int
defaultLength =
    1000


reindex : Individuals -> Individuals
reindex inds =
    { inds | content = Array.indexedMap Model.Individual.setId inds.content }
