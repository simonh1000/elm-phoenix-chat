module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, type', id)
import Html.Events exposing (onClick, onSubmit)

import Json.Encode as E

import Material
import Material.Grid as Grid exposing (grid, cell, size, offset, Device(..))
import Material.Textfield as Textfield
import Material.Card as Card
import Material.Color as Color
import Material.Button as Button
import Material.Elevation exposing (..)
import Material.Options as Options
import Material.Helpers

import Decoders exposing (..)

type alias Model =
    { username : String
    , mdl : Material.Model
    }

init = Model "" Material.model
init' s =
    Model s Material.model

type Msg
    = UpdateUserName String
    | Submit
    | Mdl (Material.Msg Msg)

-- type alias Config =
--     { mdlMessage : Material.Msg Msg -> msg }

update : (String -> msg) -> Msg -> Model -> (Model, Cmd Msg, Maybe msg)
update submitMsg message model =
    case message of
        UpdateUserName un ->
            ( { model | username = un }, Cmd.none, Nothing )
        Submit ->
            ( model, Cmd.none, Just (submitMsg model.username))
        Mdl msg ->
            let (m, c) = Material.update msg model
            in (m, c, Nothing)

view : Model -> Html Msg
view model =
    div [ id "login" ]
        [ grid [ ]
            [ cell
                [ size Desktop 6
                , size Phone 12
                , offset Desktop 3
                -- , Grid.align Grid.Middle
                ]
                [ loginCard model ]
            ]
        ]

loginCard model =
    Card.view
        [ e4 ]
        [ Card.title
            [ Color.background <| Color.color Color.Indigo Color.S600 ]
            [ h2 [] [ text "Login" ] ]
        , Card.text []
            [ text "Enter a username"
            , Html.form
                [ onSubmit Submit ]
                [ Textfield.render Mdl [0] model.mdl
                    [ Textfield.label "Chat name"
                    , Textfield.floatingLabel
                    , Textfield.value model.username
                    , Textfield.onInput UpdateUserName
                    ]
                , Button.render Mdl [1] model.mdl
                    [ Button.raised
                    , Button.ripple
                    ]
                    [ text "Join Chat"]
                ]
            ]
        ]

encoder model =
    E.object [ ("username", E.string model.username) ]
