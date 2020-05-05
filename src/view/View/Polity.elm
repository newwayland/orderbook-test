module View.Polity exposing (Polity, viewAgeCategories)

import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Model.Polity
import Update exposing (Msg(..))


{-| Re-expose the type through the view so the Main view doesn't need to see the model
-}
type alias Polity =
    Model.Polity.Polity


{-| Display and update age categories
-}
viewAgeCategories : Polity -> Block.Item Msg
viewAgeCategories settings =
    Block.custom <|
        Form.formInline []
            [ Input.number
                [ Input.id "schoolAge"
                , Model.Polity.schoolAge settings |> String.fromInt |> Input.value
                , Input.placeholder "School Age"
                , Input.small
                , Input.onInput ChangeSchoolAge
                , Input.attrs
                    [ Html.Attributes.min "0"
                    , Model.Polity.majority settings - 1 |> String.fromInt |> Html.Attributes.max
                    ]
                ]
            , Input.number
                [ Input.id "workingAge"
                , Model.Polity.majority settings |> String.fromInt |> Input.value
                , Input.placeholder "Working Age"
                , Input.small
                , Input.onInput ChangeWorkingAge
                , Input.attrs
                    [ Model.Polity.schoolAge settings + 1 |> String.fromInt |> Html.Attributes.min
                    , Model.Polity.retirementAge settings - 1 |> String.fromInt |> Html.Attributes.max
                    ]
                ]
            , Input.number
                [ Input.id "retirementAge"
                , Model.Polity.retirementAge settings |> String.fromInt |> Input.value
                , Input.placeholder "RetirementAge"
                , Input.small
                , Input.onInput ChangeRetirementAge
                , Input.attrs
                    [ Model.Polity.majority settings + 1 |> String.fromInt |> Html.Attributes.min
                    , Html.Attributes.max "120"
                    ]
                ]
            ]
