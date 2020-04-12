module View.Random exposing (Seed, viewSeedId)

import Bootstrap.Button as Button
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Html exposing (span, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Model.Random
import Update exposing (Msg(..))
import View.Extra exposing (onEnter)


type alias Seed =
    Model.Random.Seed


viewSeedId : Seed -> Block.Item Msg
viewSeedId seed =
    let
        displaySeed =
            String.fromInt seed.display
    in
    InputGroup.config
        (InputGroup.number
            [ Input.placeholder "Model Number"
            , Input.value displaySeed
            , Input.onInput ChangeSeed
            , Input.attrs [ style "max-width" "11.25em", onEnter ResetModel ]
            ]
        )
        |> InputGroup.successors
            [ InputGroup.button
                [ Button.primary, Button.attrs [ onClick ResetSeed ] ]
                [ text "Random" ]
            ]
        |> InputGroup.predecessors
            [ InputGroup.span [] [ text "Model" ] ]
        |> InputGroup.view
        |> Block.custom
