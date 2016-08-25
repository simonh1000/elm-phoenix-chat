module Main exposing (main)

import Html.App as Html
import Phoenix.Socket

import App exposing (..)

main : Program String
main =
  Html.programWithFlags
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg
