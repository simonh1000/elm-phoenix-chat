module Decoders exposing (..)

import Json.Decode exposing (..)

messageDecoder constructor =
    object2 constructor
        ("username" := string)
        (oneOf [ "body" := string, succeed ""])

messageDecoder' : (String -> a -> b) -> Decoder a -> Decoder b
messageDecoder' constructor dec =
    object2 constructor
        ("username" := string)
        ("body" := dec)

newMemberDecoder =
    object2 (,)
        ("username" := string)
        userNamesDecoder

-- JoinLobby
userNamesDecoder : Decoder (List String)
userNamesDecoder =
    "users" := list string

receiveSoundDecoder =
    object2 (,)
    ("sender" := string)
    ("body" := string)
