module View exposing (view)

import Html exposing (Html, div)
import Model exposing (Model)
import Update exposing (Msg)
import View.Clock
import View.Random


view : Model -> Html Msg
view model =
    div []
        [ View.Clock.view model.clock
        , View.Random.view model.seed
        ]
