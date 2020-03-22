module Update exposing (update)

import Model exposing (Model, Msg(..), advanceTime, decreaseSpeed, increaseSpeed)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( Model.advanceTime model, Cmd.none )

        SetTimeHere ( newZone, newTime ) ->
            ( { model | zone = newZone, time = newTime }
            , Cmd.none
            )

        IncreaseSpeed ->
            ( Model.increaseSpeed model, Cmd.none )

        DecreaseSpeed ->
            ( Model.decreaseSpeed model, Cmd.none )
