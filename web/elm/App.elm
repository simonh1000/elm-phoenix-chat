module App exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)

import Http
import Json.Decode as Json exposing ( (:=) )
import Json.Encode as E

import Task

import Material
import Material.Typography as Typo
import Material.Grid as Grid exposing (..)
-- import Material.Options as Options
import Material.Layout as Layout

import Phoenix.Socket as Socket exposing (withDebug)
import Phoenix.Channel as Channel
import Phoenix.Push as Push

import Login
import Chat
import SoundPlayer as SP

-- CONSTANTS

serverUrl = "/api/default"
socketUrl = "/socket/websocket"
roomname = "room:lobby"
-- MODEL
type ViewType
    = Login
    | Chat

type alias Model =
    { username : String
    , login : Login.Model
    , chat : Chat.Model
    , soundplayer : SP.Model
    , viewType : ViewType
    , debugMsg : String
    , phxSocket : Socket.Socket Msg
    , mdl : Material.Model
    }

type alias Flags = String

init : Flags -> (Model, Cmd Msg)
init loc =
    ( Model
        "" Login.init Chat.init SP.init Login ""
        (initPhxSocket <| Debug.log "loc" loc)
        -- (Socket.init (f ++ socketUrl))
        Material.model
    , Cmd.none
    )

initPhxSocket : String -> Socket.Socket Msg
initPhxSocket loc =
  Socket.init (loc ++ socketUrl)
    |> Socket.withDebug
    -- |> Phoenix.Socket.on "new:msg" "rooms:lobby" ReceiveChatMessage
    |> Socket.on "phx_reply" roomname ReceiveReply
    |> Socket.on "new_msg" roomname (ChatMsg << Chat.NewMessage)
    |> Socket.on "new_member" roomname (ChatMsg << Chat.NewMember)
    |> Socket.on "lost_member" roomname (ChatMsg << Chat.LostMember)
    |> Socket.on "play_sound" roomname (SPMsg << SP.Receive)


-- UPDATE

type Msg
    -- Components
    = LoginMsg Login.Msg
    | Join String
    -- Chat
    | ChatMsg Chat.Msg
    | SendChat

    | SPMsg SP.Msg
    | SendSound String String

    -- Http
    | FetchSucceed String
    | FetchFail Http.Error
    -- Channels
    | PhoenixMsg (Socket.Msg Msg)
    -- Channels - output
    | ReceiveChatMessage E.Value
    -- | ShowJoinedMessage String
    | ShowJoinedMessage E.Value
    | ReceiveReply E.Value
    -- Mdl
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        LoginMsg msg ->
            let
                (m, c1, maybeMsg) = Login.update Join msg model.login
                newModel = { model | login = m }
                c1' = Cmd.map LoginMsg c1
            in
            case maybeMsg of
                Just msg' ->
                    let (newModel', c2) = update msg' newModel
                    in (newModel', Cmd.batch [c1', c2])
                _ ->
                    (newModel, c1')
        Join username ->
            { model | username = username }
            |> joinChat
            -- let
            --     m0 = { model | username = username }
            --     (m1, c1) = joinChat m0
            --     (m2, c2) = joinSounds m1
            -- in m2 ! [ c1, c2]
        ChatMsg msg ->
            let
                (m, c1, maybeMsg) = Chat.update SendChat msg model.chat
                newModel = { model | chat = m }
                c1' = Cmd.map ChatMsg c1
            in
            case maybeMsg of
                Just msg' ->
                    let (newModel', c2) = update msg' newModel
                    in (newModel', Cmd.batch [c1', c2])
                _ ->
                    (newModel, c1')
        SendChat ->
            let
                payload = E.object [ ("body", E.string model.chat.newMessage) ]
                push =
                    Push.init "new_msg" roomname
                    |> Push.withPayload payload
                (socket', socketCmd) =
                    Socket.push push model.phxSocket
            in
            ( { model | phxSocket = socket' }
            , Cmd.map PhoenixMsg socketCmd
            )
        SPMsg msg ->
            let
                config = SP.Config model.username SendSound
                (m, c1, maybeMsg) = SP.update config msg model.soundplayer
                newModel = { model | soundplayer = m }
                c1' = Cmd.map SPMsg c1
            in
            case maybeMsg of
                Just msg' ->
                    let (newModel', c2) = update msg' newModel
                    in (newModel', Cmd.batch [c1', c2])
                _ ->
                    (newModel, c1')
        SendSound username soundfile ->
            let
                payload =
                    E.object
                        [ ("target", E.string username)
                        , ("soundfile", E.string soundfile)
                        ]
                push =
                    Push.init "send_sound" roomname
                    |> Push.withPayload payload
                (socket', socketCmd) =
                    Socket.push push model.phxSocket
            in
            ( { model | phxSocket = socket' }
            , Cmd.map PhoenixMsg socketCmd
            )
        -- ReceiveChatMessage jsonValue ->
        --     update (ChatMsg Chat.NewMessage jsonValue) model
        FetchSucceed str ->
            { model | debugMsg = str } ! []
        FetchFail err ->
            { model | debugMsg = toString err } ! []
        PhoenixMsg msg ->
            let
                _ = Debug.log "PhoenixMsg" msg
                ( phxSocket, phxCmd ) =
                    model.phxSocket
                    |> Socket.update msg
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )
        ShowJoinedMessage m ->      -- returns room name
            let _ = Debug.log "ShowJoinedMessage" m
            in { model | viewType = Chat } ! []
        Mdl msg ->
            Material.update msg model
        ReceiveReply val ->
            { model | debugMsg = toString val } ! []
        _ ->
            { model | debugMsg = toString message } ! []


-- VIEW
view : Model -> Html Msg
view model =
    Layout.render Mdl model.mdl
        [ Layout.fixedHeader
        ]
        { header =
            [ h1
                [ style [("padding", "2rem")] ]
                [ text "Simon's Elm-Phoenix Chat" ]
            ]
        , drawer = []
        , tabs = ([], [])
        , main = [ viewBody model ]
        }

viewBody : Model -> Html Msg
viewBody model =
    -- case model.viewType of
    --     Login ->
    --         Login.view model.login |> Html.map LoginMsg
    --     Chat ->
    --         viewApp model
    div [ class "main-container" ]
        [ case model.viewType of
            Login ->
                Login.view model.login |> Html.map LoginMsg
            Chat ->
                viewApp model
        , text model.debugMsg
        ]

viewApp model =
    grid [ ]
        [ cell
            [ size All 6
            , stretch
            ] [ Chat.view model.chat |> Html.map ChatMsg ]
        , cell
            [ size All 6
            , stretch
            ] [ SP.view model.chat.members model.soundplayer |> Html.map SPMsg ]
        ]
-- Commands

joinChat : Model -> (Model, Cmd Msg)
joinChat model =
    let
        payload = E.object [ ("username", E.string model.login.username) ]
        channel =
            Channel.init roomname
            |> Channel.withPayload payload
            -- |> Channel.onJoin (always (ShowJoinedMessage roomname))
            |> Channel.onJoin ShowJoinedMessage
        (socket', cmd) = Socket.join channel model.phxSocket
    in
    ( { model | phxSocket = socket' }
    , Cmd.map PhoenixMsg cmd
    )

-- joinSounds : Model -> (Model, Cmd Msg)
-- joinSounds model =
--     let
--         channel =
--             Channel.init <| "sounds:" ++ model.login.username
--         (socket', cmd) = Socket.join channel model.phxSocket
--     in
--     ( { model | phxSocket = socket' }
--     , Cmd.map PhoenixMsg cmd
--     )
