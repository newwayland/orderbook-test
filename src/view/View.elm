module View exposing (view)

import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html exposing (Html, div, node)
import Html.Attributes exposing (attribute, name, style)
import Model exposing (Model)
import Update exposing (Msg)
import View.Clock
import View.Individuals
import View.Random


view : Model -> Html Msg
view model =
    div [ style "max-width" "375px" ]
        [ viewPortHintWorkaround
        , CDN.stylesheet
        , CDN.fontAwesome
        , Grid.containerFluid
            []
            [ View.Random.view model.seed
            , View.Clock.view model.clock
            , View.Individuals.view model.individuals (View.Clock.displayBirthDate model.clock) (View.Clock.displayAge model.clock)
            ]
        ]


viewPortHintWorkaround : Html msg
viewPortHintWorkaround =
    node "meta"
        [ name "viewport", attribute "content" "width=device-width, initial-scale=1.0" ]
        []
