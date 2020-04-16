module Model.TaskManager exposing (advanceTime)

import Model.Clock exposing (Clock)
import Model.Individual exposing (Individual)
import Model.Individuals exposing (Individuals)


type alias ModelElements a =
    { a | clock : Clock, individuals : Individuals }


advanceTime : ModelElements a -> ModelElements a
advanceTime model =
    { model
        | individuals =
            Model.Individuals.map
                (manageIndividual model.clock)
                model.individuals
    }


manageIndividual : Clock -> Individual -> Individual
manageIndividual clock ind =
    let
        age =
            Model.Individual.birthDate ind |> Model.Clock.age clock |> Model.Clock.yearIntToInt

        message =
            if age >= 65 then
                "Retired"

            else if age <= 18 then
                "At School"

            else
                "Working"

        logMessage =
            Model.Clock.toDateTime clock |> dateTagView >> Model.Individual.addJournalEntry message
    in
    logMessage ind



-- HELPERS


{-| Format the current clock as a string for use in journal entries
-}
dateTagView : Model.Clock.DateTime -> String
dateTagView dateTime =
    let
        year =
            String.fromInt dateTime.year |> String.padLeft 4 '0'

        month =
            Model.Clock.toDisplayMonth dateTime.month

        day =
            String.fromInt dateTime.day |> String.padLeft 2 ' '
    in
    [ day, month, year ] |> String.join " "
