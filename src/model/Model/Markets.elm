module Model.Markets exposing
    ( Markets
    , empty
    , labourMarket, productMarket, updateLabourMarket, updateProductMarket
    )

{-| A representation of a set of market orderbooks


# Definition

@docs Markets


# Updaters

@docs empty

-}

import Array exposing (Array)
import List.Extra
import Model.Market exposing (Market)
import OrderBook exposing (OrderBook)


type alias Markets =
    { current : Int
    , content : MarketArray
    }


type alias MarketArray =
    Array Market


marketList : List String
marketList =
    [ "labour", "products" ]


{-| A set of markets with empty OrderBooks
-}
empty : Markets
empty =
    marketList
        |> List.indexedMap (\index str -> Model.Market.default index str)
        |> Array.fromList
        |> Markets 0


update : String -> OrderBook -> Markets -> Markets
update key newBook oldMarkets =
    let
        maybeIndex =
            getIndex key
    in
    { oldMarkets
        | content =
            maybeIndex
                |> Maybe.andThen (\index -> Array.get index oldMarkets.content)
                |> Maybe.map2 (\index market -> Array.set index (Model.Market.update newBook market) oldMarkets.content) maybeIndex
                |> Maybe.withDefault oldMarkets.content
    }


getIndex : String -> Maybe Int
getIndex key =
    List.Extra.elemIndex key marketList


maybeGet : String -> Markets -> Maybe Market
maybeGet key markets =
    getIndex key
        |> Maybe.andThen (\x -> Array.get x markets.content)


getBook : String -> Markets -> OrderBook
getBook key markets =
    maybeGet key markets
        |> Maybe.map (\x -> x.book)
        |> Maybe.withDefault OrderBook.empty


labourMarket : Markets -> OrderBook
labourMarket =
    getBook "labour"


productMarket : Markets -> OrderBook
productMarket =
    getBook "products"


updateLabourMarket : OrderBook -> Markets -> Markets
updateLabourMarket =
    update "labour"


updateProductMarket : OrderBook -> Markets -> Markets
updateProductMarket =
    update "products"
