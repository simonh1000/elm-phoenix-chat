module App exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)

import Http
-- import Json.Decode as Json exposing ( (:=) )
import Json.Encode exposing (Value, null)

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
import SocketHelpers as SH exposing (..)

-- CONSTANTS

serverUrl = "/api/default"
lobby = "room:lobby"
-- MODEL
type ViewType
    = Login
    | Chat

type alias Model =
    { username : String
    , login : Login.Model
    , chat : Chat.Model
    , soundplayer : Maybe SP.Model
    , viewType : ViewType
    , debugMsg : String
    , phxSocket : Socket.Socket Msg
    , mdl : Material.Model
    }

type alias Flags = String

init : Flags -> (Model, Cmd Msg)
init loc =
    ( Model
        "" Login.init Chat.init Nothing Login "initial debugMsg"
        (initPhxSocket <| Debug.log "loc" loc)
        -- (Socket.init (f ++ socketUrl))
        Material.model
    , Cmd.none
    )

initPhxSocket : String -> Socket.Socket Msg
initPhxSocket loc =
    SH.init loc

-- UPDATE

type Msg
    -- Login Page and follow-on messages
    = LoginMsg Login.Msg
    | Signin String
    | JoinLobby Value
    -- Chat Page and follow-on messages
    | ChatMsg Chat.Msg
    | SendChat Value
    | ReceiveChatMessage Value

    | SPMsg SP.Msg
    | SendSound Value
    | MyChannelSuccess Value

    -- Channels
    | PhoenixMsg (Socket.Msg Msg)
    -- Channels - output
    | ShowJoinedMessage Value
    | JoinError String Value
    -- Mdl
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        -- Login Page
        LoginMsg msg ->
            let
                (m1, c1, maybeMsg) = Login.update Signin msg model.login
                res1 = ({ model | login = m1 }, Cmd.map LoginMsg c1)
            in bind' res1 update maybeMsg
        Signin username ->
            joinChat { model | username = username } `bind` makeIndividualChannel
        JoinLobby val ->
            { model | viewType = Chat }
            |> update (ChatMsg <| Chat.JoinLobby val)
        MyChannelSuccess _ ->
            { model | soundplayer = Just SP.init } ! []

        ChatMsg msg ->
            let
                (m1, c1, maybeMsg) = Chat.update SendChat msg model.chat
                res1 = ({ model | chat = m1 }, Cmd.map ChatMsg c1)
            in bind' res1 update maybeMsg
        SendChat payload ->
            let
                push =
                    Push.init "new_msg" lobby
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
                res = Maybe.map (SP.update config msg) model.soundplayer
            in case res of
                Just (m, c1, maybeMsg) ->
                    bind' ({model | soundplayer = Just m }, c1 |> Cmd.map SPMsg) update maybeMsg
                Nothing ->
                    model ! []
        SendSound payload ->
            let
                push =
                    Push.init "send_sound" lobby
                    |> Push.withPayload payload
                (socket', socketCmd) =
                    Socket.push push model.phxSocket
            in
            ( { model | phxSocket = socket' }
            , Cmd.map PhoenixMsg socketCmd
            )
        PhoenixMsg msg ->
            let
                _ = Debug.log "App.update PhoenixMsg" msg
                ( phxSocket, phxCmd ) =
                    Socket.update msg model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )
        Mdl msg ->
            Material.update msg model
        _ ->
            { model | debugMsg = toString message } ! []


-- VIEW
view : Model -> Html Msg
view model =
    Layout.render Mdl model.mdl
        [ Layout.fixedHeader
        ]
        { header =
            [ h1 [] [ text "Simon's Elm-Phoenix Chat" ] ]
        , drawer = []
        , tabs = ([], [])
        , main = [ viewBody model ]
        }

viewBody : Model -> Html Msg
viewBody model =
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
    let payload = Login.encoder model.login
    in joinChannel payload lobby JoinLobby (JoinError lobby)
        [ ("new_msg", ChatMsg << Chat.NewMessage)
        , ("new_member", ChatMsg << Chat.NewMember)
        , ("lost_member", ChatMsg << Chat.LostMember)
        ] PhoenixMsg model

makeIndividualChannel : Model -> (Model, Cmd Msg)
makeIndividualChannel model =
    let
        payload = null
        myRoom = "user:" ++ model.login.username
    in joinChannel payload myRoom MyChannelSuccess (JoinError myRoom)
        [ ("receive_sound", SPMsg << SP.Receive)
        ] PhoenixMsg model

bind : (Model, Cmd Msg) -> (Model -> (Model, Cmd Msg)) -> (Model, Cmd Msg)
bind (m1, c1) f =
    let (m2, c2) = f m1
    in (m2, Cmd.batch [c1, c2])

bind' : (Model, Cmd msg) -> (msg -> Model -> (Model, Cmd msg)) -> Maybe msg -> (Model, Cmd msg)
bind' (md1, c1) update mms =
    case mms of
        Just ms ->
            let (md2, c2) = update ms md1
            in (md2, Cmd.batch [c1, c2])
        Nothing ->
            (md1, c1)
