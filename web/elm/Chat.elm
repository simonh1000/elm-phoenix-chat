module Chat exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Date exposing (Date)

import Platform.Cmd exposing (Cmd)

type alias Model =
    { messages : Messages
    , newMessage : String
    }

init = Model [] ""

type alias Messages =
    List Message

type alias Message =
    { time : Date
    , sender : String
    , message : String
    }

type Msg
    = Send
    | NewMessage String

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    model ! []

view : Model -> Html Msg
view model =
    text "tbc"
