module Main exposing (main)

import Browser
import Model exposing (Model)
import Subscriptions
import Update exposing (Msg(..))
import View


main : Program () Model Msg
main =
    Browser.element
        { init = Update.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }
