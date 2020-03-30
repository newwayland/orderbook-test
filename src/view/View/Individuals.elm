module View.Individuals exposing (view)

import Html exposing (Html, div, span, text)
import Html.Events
import Model.Individual exposing (Individual, Individuals)
import Update exposing (Msg(..))


view : Individuals -> Html Msg
view individuals =
    let
        currentIndividual =
            Model.Individual.current individuals
    in
    div []
        [ div []
            [ span []
                [ text "Name: "
                ]
            , span []
                [ text currentIndividual.name
                ]
            ]
        , div []
            [ span []
                [ text "Age: "
                ]
            , span []
                [ String.fromInt currentIndividual.age |> text ]
            ]
        ]
