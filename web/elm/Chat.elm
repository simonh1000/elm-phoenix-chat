module Chat exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode exposing (decodeValue, Value)
import Json.Encode as E
import String
import List as L
import Date exposing (Date)
import Platform.Cmd exposing (Cmd)

import Material
import Material.Grid as Grid exposing (..)
import Material.Textfield as Textfield
import Material.Button as Button
import Material.List as MList
import Material.Helpers exposing (..)
import Material.Options as Options exposing (css)
import Material.Color as Color
import Material.Typography as Typo
import Material.Toggles as Toggles
import Material.Button as Button
import Material.Options exposing (cs)
import Material.Icon as Icon

import Ports exposing (..)
import Decoders exposing (..)
import Common exposing (..)

type alias Model =
    { messages : Messages
    , newMessage : String
    , members : List String
    , soundplayer : Bool
    , mdl : Material.Model
    }

init = Model [] "" [] False Material.model

type alias Member = String

type alias Messages =
    List Message

type alias Message =
    { userName : String
    , body : String
    -- , time : Date
    }

type Msg
    = JoinLobby Value
    | LoadSoundPlayer Value
    | NewMessage Value
    | NewMember Value
    | LostMember Value
    | SendChat
    | UpdateInput String

    | SendSound String String
    | Receive Value

    | Mdl (Material.Msg Msg)

type alias Config msg =
    { sendMsg : E.Value -> msg
    , toastMsg : String -> msg
    , sendSoundMsg : E.Value -> msg
    }

update : Config msg -> Msg -> Model -> (Model, Cmd Msg, Maybe msg)
update config message model =
    case message of
        LoadSoundPlayer _ ->
            { model | soundplayer = True } |> nothing
        JoinLobby val ->
            case decodeValue userNamesDecoder val of
                Result.Ok members ->
                    { model | members = members } |> nothing
                Result.Err err ->
                    (model, Cmd.none, Just <| config.toastMsg <| "Join: " ++ err)
        NewMessage jsonMessage ->
            case decodeValue (messageDecoder Message) jsonMessage of
                Result.Ok newMsg ->
                    { model
                            | messages = newMsg :: model.messages
                            , newMessage = "" } |> nothing
                Result.Err err ->
                    (model, Cmd.none, Just <| config.toastMsg <| "NewMessage: " ++ err)

        NewMember jsonMessage ->
            case decodeValue newMemberDecoder (Debug.log "*NewMember" jsonMessage) of
                Result.Ok (newUsername, members) ->
                    let newMessage = Message "system" <| newUsername ++ " joined"
                    in
                    { model
                        | messages = newMessage :: model.messages
                        , members = members } |> nothing
                Result.Err err ->
                    (model, Cmd.none, Just <| config.toastMsg <| "NewMember: " ++ err)

        LostMember jsonMessage ->
            case decodeValue (messageDecoder Message) jsonMessage of
                Result.Ok lostMember ->
                    let newMessage = Message "system" <| lostMember.userName ++ " left"
                    in
                    { model
                        | messages = newMessage :: model.messages
                        , members = model.members |> L.filter ((/=) lostMember.userName) } |> nothing
                Result.Err err ->
                    (model, Cmd.none, Just <| config.toastMsg err)

        UpdateInput s ->
            ({ model | newMessage = s}, Cmd.none, Nothing)
        SendChat ->
            let payload = E.object [ ("body", E.string model.newMessage) ]
            in (model, Cmd.none, Just <| config.sendMsg payload)
        Receive jsonMessage ->
            case decodeValue receiveSoundDecoder jsonMessage of
                Result.Ok (sender, soundfile) ->
                    (model, player soundfile, Just <| config.toastMsg ("LOL from " ++ sender))
                Result.Err err ->
                    (model, Cmd.none, Just <| config.toastMsg err)
        SendSound user sound ->
            let payload = encodeSendSound user sound
            in (model, Cmd.none, Just <| config.sendSoundMsg payload)
        Mdl msg ->
            let (m, c) = Material.update msg model
            in (m, c, Nothing)


view : Model -> Html Msg
view model =
    grid []
        [ chatComponent model
        , soundplayerCmp model
        ]

-- L H S

chatComponent model =
    cell
        [ Grid.size All 6
        , stretch
        , cs "chat"
        ]
        [ model.messages |> L.reverse |> L.map viewMessage |> div [ class "messages" ]
        , inputView model
        ]

viewMessage : Message -> Html Msg
viewMessage msg =
    MList.li []
        [ Options.div
            [ Options.center
            , Color.background (Color.color Color.Green Color.S500)
            , Color.text Color.accentContrast
            , Typo.title
            , css "width" "36px"
            , css "height" "36px"
            , css "margin-right" "2rem"
            ]
            [text (msg.userName |> String.left 1) ]
        , MList.content []
            [ span [ class <| "user-name " ++ msg.userName] [ text msg.userName ]
            , span [ class "message" ] [ text <| ": " ++ msg.body ]
            ]
        ]

inputView model =
    Html.form
        [ onSubmit SendChat
        , class "message-input"
        ]
        [ Textfield.render Mdl [2, 0] model.mdl
            [ Textfield.label "Type message"
            , Textfield.floatingLabel
            , Textfield.value model.newMessage
            , Textfield.onInput UpdateInput
            ]
        ,  Button.render Mdl [2, 1] model.mdl
            [ Button.fab
            , Button.colored
            , Button.ripple
            , if model.newMessage == "" then
                Button.disabled
              else
                Button.raised
            ] [ Icon.i "send" ]
        ]

-- R H S

soundplayerCmp : Model -> Cell Msg
soundplayerCmp model =
    cell
        [ Grid.size All 6
        , stretch
        , cs "sound-player"
        ] <|
        if model.soundplayer then
            [ viewSoundPlayer model ]
        else
            [ membersView model.members
            , text "More functionality will become available when the user-channel is available"
            ]


membersView users =
    let
        item t =
            Options.styled p
                [ Typo.subhead ]
                [ text t ]
    in
    ul [ class "users-list" ] <|
        L.map item users


viewSoundPlayer : Model -> Html Msg
viewSoundPlayer {members, mdl} =
    let
        radioToggle idx member =
            span []
                [ Button.render Mdl [3, idx] mdl
                    [ Button.ripple
                    , Button.primary
                    , Button.raised
                    , Button.onClick (SendSound member "guitar")
                    ]
                    [ text member ]
                ]
    in
        div []
            [ h3 [] [ text "Surprise your colleagues" ]
            , L.indexedMap radioToggle members |> div []
            ]


encodeSendSound : String -> String -> E.Value
encodeSendSound username soundfile =
    E.object
        [ ("target", E.string username)
        , ("soundfile", E.string soundfile)
        ]
