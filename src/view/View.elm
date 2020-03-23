module View exposing (view)

import Html exposing (Html, div)
import Model exposing (Model)
import Update exposing (Msg)
import View.Clock


view : Model -> Html Msg
view model =
    div []
        [ View.Clock.view model.clock
        ]
