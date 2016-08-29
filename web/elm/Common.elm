module Common exposing (..)

import Platform.Cmd exposing (Cmd)

nothing m =
    (m, Cmd.none, Nothing)


bind : (a -> (a, Cmd msg)) -> (a, Cmd msg) -> (a, Cmd msg)
bind f (m1, c1) =
    let (m2, c2) = f m1
    in (m2, Cmd.batch [c1, c2])

bind' : (a, Cmd msg) -> (msg ->a -> (a, Cmd msg)) -> Maybe msg -> (a, Cmd msg)
bind' initialValues update mms =
    case mms of
        Just ms ->
            bind (update ms) initialValues
        Nothing ->
            initialValues
