module Util exposing (appendErrors, dateStringDecoder, onClickStopPropagation, pair, viewIf)

import Html exposing (Attribute, Html)
import Html.Events exposing (stopPropagationOn)
import Iso8601
import Json.Decode as Decode exposing (Decoder, fail, succeed)
import Parser
import Time


{-| Useful when building up a Cmd via a pipeline, and then pairing it with
a model at the end.

    session.user
        |> User.Request.foo
        |> Task.attempt Foo
        |> pair { model | something = blah }

-}
pair : a -> b -> ( a, b )
pair first second =
    ( first, second )


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content

    else
        Html.text ""


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))


appendErrors : { model | errors : List error } -> List error -> { model | errors : List error }
appendErrors model errors =
    { model | errors = model.errors ++ errors }


{-| Decode an ISO-8601 date string.
-}
dateStringDecoder : Decoder Time.Posix
dateStringDecoder =
    Decode.string
        |> Decode.andThen (\str -> fromResult str (Iso8601.toTime str))


fromResult : String -> Result (List Parser.DeadEnd) a -> Decoder a
fromResult source result =
    case result of
        Ok successValue ->
            succeed successValue

        Err _ ->
            fail ("Failed to parse: " ++ source)
