module SoundPlayer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as Json exposing (decodeValue, Value)
import Json.Encode as E
import List as L

import Material
import Material.Toggles as Toggles
import Material.Button as Button

import Ports exposing (..)
import Decoders exposing (..)
import Common exposing (..)

type alias User = String
type alias Sound = String

-- type alias Model =
--     { selectedUser : User
--     , mdl : Material.Model
--     }

type alias PlayCommand =
    { target : String
    , soundfile : String
    }

-- init = Model "" Material.model

type Msg
    = Send User Sound
    | Receive Value

type alias Config msg =
    { username : String
    , sendMsg : E.Value -> msg
    , errorMsg : String -> msg
    }

update : Config msg -> Msg -> (Cmd Msg, Maybe msg)
update config message =
    case Debug.log "SP.update" message of
        Receive jsonMessage ->
            case decodeValue receiveSoundDecoder jsonMessage of
                Result.Ok soundfile ->
                    (player soundfile, Nothing)
                Result.Err err ->
                    (Cmd.none, err |> toString |> config.errorMsg |> Just)
        Send user sound ->
            let payload = encodeSendSound user sound
            in (Cmd.none, Just <| config.sendMsg payload)
-- update : Config msg -> Msg -> Model -> (Model, Cmd Msg, Maybe msg)
-- update config message model =
--     case Debug.log "SP.update" message of
--         Receive jsonMessage ->
--             case decodeValue receiveSoundDecoder jsonMessage of
--                 Result.Ok soundfile ->
--                     (model, player soundfile, Nothing)
--                 Result.Err err ->
--                     model |> nothing
--         Send user sound ->
--             let payload = encodeSendSound user sound
--             in (model, Cmd.none, Just <| config.sendMsg payload)
--         Mdl msg ->
--             let (m, c) = Material.update msg model
--             in (m, c, Nothing)

-- view : List User -> Maybe Model -> Html Msg
view mdlMsg mdlModel users model =
    if model then
        viewMain mdlMsg mdlModel users
    else
        text "This component will shown when the user-channel is available"

-- viewMain : List User -> Model -> Html Msg
viewMain mdlMsg mdlModel users =
    let
        radioToggle idx user =
            Button.render mdlMsg [3, idx] mdlModel
                [ Button.ripple
                , Button.raised
                , Button.onClick (Send user "guitar")
                ]
                [ text user ]
    in
        div [] <|
            L.indexedMap radioToggle users

encodeSendSound : String -> String -> E.Value
encodeSendSound username soundfile =
    E.object
        [ ("target", E.string username)
        , ("soundfile", E.string soundfile)
        ]
