module View.Random exposing (view)

import Html exposing (Html, div, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Model.Random exposing (Seed)
import Update exposing (Msg(..))


view : Seed -> Html Msg
view seed =
    div []
        [ input
            [ placeholder "Model Number", value (String.fromInt seed.display), onInput ChangeSeed ]
            []
        ]
