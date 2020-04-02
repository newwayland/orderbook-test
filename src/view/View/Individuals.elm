module View.Individuals exposing (view)

import Html exposing (Html, div, span, text)
import Html.Events
import Model.Individual exposing (Individual, Individuals)
import Model.Types exposing (BirthDate)
import Update exposing (Msg(..))


view : Individuals -> (BirthDate -> String) -> Html Msg
view individuals displayAge =
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
                [ text <| displayAge currentIndividual.birthdate ]
            ]
        ]
