module Model.TaskManager exposing (advanceTime)

import Array
import Model.Clock exposing (Clock, TimeOfDay(..))
import Model.Cursor
import Model.Individual exposing (Individual, Logger)
import Model.Individuals exposing (Individuals, IndividualsArray)
import Model.Markets exposing (Markets)
import Model.Polity exposing (Polity)
import Model.Types exposing (AgeCategory(..), YearInt)
import OrderBook exposing (Event, Order, OrderBook, OrderRequest)
import String.Conversions



{- The section of the model the task manager works with -}


type alias ModelElements a =
    { a
        | clock : Clock
        , individuals : Individuals
        , markets : Markets
        , polity : Polity
    }



{- Elements that are updated by the tasks -}


type alias IndividualUpdateables =
    { individual : Individual
    , labour : OrderBook
    , products : OrderBook
    }


type alias TransactionResults =
    ( OrderBook, IndividualsArray )


{-| Move the model on for the current time of day
-}
advanceTime : ModelElements a -> ModelElements a
advanceTime model =
    let
        timeOfDay =
            Model.Clock.toTimeOfDay model.clock

        logIt =
            Model.Clock.toDateTime model.clock |> dateTagView >> Model.Individual.addJournalEntry

        loopIndividuals funcActivity funcModel =
            Model.Cursor.foldl
                (processIndividualActivity (funcActivity logIt))
                (funcModel model)
                model.individuals
    in
    case timeOfDay of
        Morning ->
            loopIndividuals morningActivity morningModel

        Midday ->
            loopIndividuals middayActivity middayModel

        Evening ->
            loopIndividuals eveningActivity eveningModel
                |> clearMarkets logIt


{-| Settle the days events and unmatched orders on the markets
-}
clearMarkets : Logger -> ModelElements a -> ModelElements a
clearMarkets logIt model =
    let
        labour =
            Model.Markets.labourMarket model.markets

        labourHoursLog =
            Model.Individual.unworkedHoursLog >> logIt

        products =
            Model.Markets.productMarket model.markets
    in
    { model
        | individuals =
            model.individuals.content
                |> clearEvents logIt labour
                |> clearEvents logIt products
                |> clearSellOrders labourHoursLog labour
                |> Model.Cursor.updateFromArray model.individuals
    }


type alias LogAmount =
    Int -> Individual -> Individual


{-| Log all the failed sell orders in the journals of the indviduals
-}
clearSellOrders : LogAmount -> OrderBook -> IndividualsArray -> IndividualsArray
clearSellOrders logAmount book inds =
    OrderBook.sellOrders book
        |> List.foldl (clearSellOrder logAmount) inds


{-| Log the failed sell order in the individual's journal
-}
clearSellOrder : LogAmount -> Order -> IndividualsArray -> IndividualsArray
clearSellOrder logAmount order oldIndividuals =
    case
        Array.get order.trader oldIndividuals
            |> Maybe.map (logAmount order.quantity)
    of
        Just individual ->
            Array.set order.trader individual oldIndividuals

        Nothing ->
            oldIndividuals


{-| Settle the cash value of the transactions undertaken on the order book
-}
clearEvents : Logger -> OrderBook -> IndividualsArray -> IndividualsArray
clearEvents logIt book inds =
    OrderBook.events book
        |> List.foldl (clearEvent logIt) inds


{-| Settle a transaction between a buyer and a seller - updating their journals
-}
clearEvent : Logger -> Event -> IndividualsArray -> IndividualsArray
clearEvent logIt event oldIndividuals =
    let
        total =
            event.price * event.quantity

        buyer =
            Array.get event.buyer oldIndividuals

        maybeUpdated =
            Array.get event.seller oldIndividuals
                |> Maybe.map2 (Model.Individual.transferCash logIt total) buyer
    in
    case maybeUpdated of
        Just ( newBuyer, newSeller ) ->
            Array.set event.buyer newBuyer oldIndividuals
                |> Array.set event.seller newSeller

        Nothing ->
            oldIndividuals


{-| On a morning start with clear markets and an empty Individuals accumulator
-}
morningModel : ModelElements a -> ModelElements a
morningModel model =
    { model
        | individuals = Model.Cursor.indexedEmpty model.individuals
        , markets = Model.Markets.empty |> Model.Cursor.retainIndex model.markets
    }


middayModel : ModelElements a -> ModelElements a
middayModel model =
    { model
        | individuals = Model.Cursor.indexedEmpty model.individuals
    }


eveningModel : ModelElements a -> ModelElements a
eveningModel =
    middayModel


{-| Run an activity processor across an individual and update the world state resulting from that interaction
-}
processIndividualActivity :
    (AgeCategory -> Int -> IndividualUpdateables -> IndividualUpdateables)
    -> Individual
    -> ModelElements a
    -> ModelElements a
processIndividualActivity processor individual model =
    let
        ageCategory =
            age
                model.clock
                individual
                |> Model.Polity.categoriseAge model.polity

        updates =
            IndividualUpdateables
                individual
                (Model.Markets.labourMarket model.markets)
                (Model.Markets.productMarket model.markets)
                |> processor
                    ageCategory
                    (Model.Individual.id individual)

        updateMarket =
            Model.Markets.updateLabourMarket updates.labour >> Model.Markets.updateProductMarket updates.products
    in
    { model | individuals = Model.Cursor.push updates.individual model.individuals, markets = updateMarket model.markets }


{-| Get each individual to indicate what they want out of the day
-}
morningActivity : Logger -> AgeCategory -> Int -> IndividualUpdateables -> IndividualUpdateables
morningActivity logIt ageCategory index =
    offerWork logIt ageCategory index >> askOutput logIt ageCategory index


{-| Let each operational organisation try and match the wants and needs
-}
middayActivity : Logger -> AgeCategory -> Int -> IndividualUpdateables -> IndividualUpdateables
middayActivity =
    buyWork


eveningActivity : Logger -> AgeCategory -> Int -> IndividualUpdateables -> IndividualUpdateables
eveningActivity logIt ageCategory _ updateables =
    { updateables
        | individual = Model.Individual.receivePension logIt ageCategory updateables.individual
    }


{-| Add an optional offer of work to the market, and log that with the individuak
-}
offerWork : Logger -> AgeCategory -> Int -> IndividualUpdateables -> IndividualUpdateables
offerWork logIt ageCategory index updateables =
    let
        ( timeOffer, timeLog ) =
            Model.Individual.offeredHours ageCategory updateables.individual
    in
    { updateables
        | individual = logIt timeLog updateables.individual
        , labour =
            case timeOffer of
                Just desiredWork ->
                    OrderBook.sell
                        (OrderRequest index desiredWork.quantity (Just desiredWork.price))
                        updateables.labour

                Nothing ->
                    updateables.labour
    }


{-| Add an optional request for output to the market, and log that with the individuak
-}
askOutput : Logger -> AgeCategory -> Int -> IndividualUpdateables -> IndividualUpdateables
askOutput logIt ageCategory index updateables =
    let
        ( productBid, productLog ) =
            Model.Individual.productAsk ageCategory updateables.individual
    in
    { updateables
        | individual = logIt productLog updateables.individual
        , products =
            case productBid of
                Just desiredProduct ->
                    OrderBook.buy
                        (OrderRequest index desiredProduct.quantity (Just desiredProduct.price))
                        updateables.products

                Nothing ->
                    updateables.products
    }


buyWork : Logger -> AgeCategory -> Int -> IndividualUpdateables -> IndividualUpdateables
buyWork logIt ageCategory index =
    identity



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
