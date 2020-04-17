module Model.TaskManager exposing (advanceTime)

import Model.Clock exposing (Clock, TimeOfDay(..))
import Model.Individual exposing (Individual)
import Model.Individuals exposing (Individuals)


type alias ModelElements a =
    { a | clock : Clock, individuals : Individuals }


advanceTime : ModelElements a -> ModelElements a
advanceTime model =
    let
        timeOfDay =
            Model.Clock.toTimeOfDay model.clock

        logIt =
            Model.Clock.toDateTime model.clock |> dateTagView >> Model.Individual.addJournalEntry

        manageIndividual =
            case timeOfDay of
                Night ->
                    logIt "Night"

                Evening ->
                    logIt "Relaxing"

                Midday ->
                    manageWorkDay logIt model.clock
    in
    { model
        | individuals =
            Model.Individuals.map
                manageIndividual
                model.individuals
    }


manageWorkDay :
    (String -> Individual -> Individual)
    -> Clock
    -> Individual
    -> Individual
manageWorkDay logIt clock ind =
    let
        currentAge =
            age clock ind

        message =
            if currentAge >= 65 then
                "Retired"

            else if currentAge <= 5 then
                "At Nursery"

            else if currentAge <= 18 then
                "At School"

            else
                "Working"
    in
    logIt message ind



-- HELPERS


{-| Calculate the age of an individual in years
-}
age : Clock -> Individual -> Int
age clock ind =
    Model.Individual.birthDate ind |> Model.Clock.age clock |> Model.Clock.yearIntToInt


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
