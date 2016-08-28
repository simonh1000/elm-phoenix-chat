module Common exposing (..)

import Platform.Cmd exposing (Cmd)

nothing m =
    (m, Cmd.none, Nothing)
