module View.Individuals exposing (view)

import Html exposing (Html, div, span, text)
import Model.Individual exposing (Individuals, Sex(..))
import Model.Types exposing (BirthDate)
import Update exposing (Msg(..))


view : Individuals -> (BirthDate -> String) -> (BirthDate -> String) -> Html Msg
view individuals displayBirthDate displayAge =
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
                [ text "Sex: "
                ]
            , span []
                [ text <| displaySex currentIndividual.sex ]
            ]
        , div []
            [ span []
                [ text "BirthDate: "
                ]
            , span []
                [ text <| displayBirthDate currentIndividual.birthdate ]
            ]
        , div []
            [ span []
                [ text "Age: "
                ]
            , span []
                [ text <| displayAge currentIndividual.birthdate ]
            ]
        ]


displaySex : Model.Individual.Sex -> String
displaySex sex =
    case sex of
        Male ->
            "Male"

        Female ->
            "Female"
