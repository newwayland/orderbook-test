module Model.TaskManager exposing (advanceTime)

import Model.Clock exposing (Clock, TimeOfDay(..), YearInt, isNurseryAge, isRetired, isSchoolAge, isWorkingAge)
import Model.Individual exposing (Individual)
import Model.Individuals exposing (Individuals)
import String.Conversions



{- The section of the model the task manager works with -}


type alias ModelElements a =
    { a | clock : Clock, individuals : Individuals }



{- A function that logs a message in the individuals journal -}


type alias Logger =
    String -> Individual -> Individual



{- Move the model on for the current time of day -}


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
                    -- bidForOutput
                    logIt
                        "Night"

                Evening ->
                    offerHours model.clock logIt

                Midday ->
                    manageWorkDay model.clock logIt
    in
    { model
        | individuals =
            Model.Individuals.map
                manageIndividual
                model.individuals
    }


offerHours :
    Clock
    -> Logger
    -> Individual
    -> Individual
offerHours clock logIt ind =
    let
        currentAge =
            age clock ind

        updatedHours =
            if isWorkingAge currentAge then
                Model.Individual.offer Model.Individual.defaultWorkingHours ind

            else
                Model.Individual.offer Model.Individual.retiredWorkingHours ind

        offer =
            "Offered " ++ (Model.Individual.workHoursOffered updatedHours |> String.fromFloat) ++ " Hours of Work"
    in
    logIt offer updatedHours



{- Perform an individual's activities during the working part of the day -}


manageWorkDay :
    Clock
    -> Logger
    -> Individual
    -> Individual
manageWorkDay clock logIt ind =
    let
        currentAge =
            age clock ind
    in
    if isWorkingAge currentAge then
        poolHours logIt ind

    else
        selfConsumeHours currentAge logIt ind



{- For working individuals add your output to the tradeable pool -}


poolHours :
    Logger
    -> Individual
    -> Individual
poolHours logIt =
    logIt "Working"



{- For non-working individuals create output for yourself -}


selfConsumeHours :
    YearInt
    -> Logger
    -> Individual
    -> Individual
selfConsumeHours currentAge logIt =
    if isNurseryAge currentAge then
        logIt "At Nursery"

    else if isSchoolAge currentAge then
        logIt "At School"

    else if isRetired currentAge then
        logIt "Retired"

    else
        logIt "Unemployed"



-- HELPERS


{-| Calculate the age of an individual in years
-}
age : Clock -> Individual -> YearInt
age clock =
    Model.Individual.birthDate >> Model.Clock.age clock


{-| Format the current clock as a string for use in journal entries
-}
dateTagView : Model.Clock.DateTime -> String
dateTagView dateTime =
    let
        year =
            String.fromInt dateTime.year |> String.padLeft 4 '0'

        month =
            String.Conversions.fromMonth dateTime.month

        day =
            String.fromInt dateTime.day |> String.padLeft 2 ' '
    in
    [ day, month, year ] |> String.join " "
