module SocketHelpers exposing (..)

import List as L

import Phoenix.Socket as Socket exposing (withDebug)
import Phoenix.Channel as Channel
import Phoenix.Push as Push

import Json.Encode exposing (Value)

socketUrl = "/socket/websocket"

init : String -> Socket.Socket msg
init loc =
    Socket.init (loc ++ socketUrl)
    |> Socket.withDebug


joinChannel : Value
    -> String
    -> (Value -> msg)
    -> (Value -> msg)
    -> List ( String, Value -> msg )
    -> (Socket.Msg msg -> msg)
    -> { m | phxSocket : Socket.Socket msg }
    -> ( { m | phxSocket : Socket.Socket msg }, Cmd msg )
joinChannel payload roomname joinSuccessMsg joinErrorMsg hooks phxMsg model =
    let
        channel =
            Channel.init roomname
            |> Channel.withPayload payload
            |> Channel.onJoin joinSuccessMsg
            |> Channel.onError joinErrorMsg
        (socket', cmd) =
            Socket.join channel model.phxSocket
        socket'' =
            L.foldl
                (\(name, msg) -> Socket.on name roomname msg)
                socket'
                hooks
    in
    ( { model | phxSocket = socket'' }
    , Cmd.map phxMsg cmd
    )

pushSocket msgType topic phxMsg payload model =
    let
        push =
            Push.init msgType topic
            |> Push.withPayload payload
        (socket', socketCmd) =
            Socket.push push model.phxSocket
    in
    ( { model | phxSocket = socket' }
    , Cmd.map phxMsg socketCmd
    )
