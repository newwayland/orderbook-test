module Model.Market exposing (Market, default, update)

import OrderBook exposing (OrderBook)


type alias Market =
    { id : Int
    , name : String
    , book : OrderBook
    }


default : Int -> String -> Market
default index name =
    Market index name OrderBook.empty


update : OrderBook -> Market -> Market
update newBook oldMarket =
    { oldMarket | book = newBook }
