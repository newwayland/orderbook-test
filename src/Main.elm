module Main exposing (..)

import Browser
import Model
import Subscriptions
import Update
import View


main =
    Browser.element
        { init = Model.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }
