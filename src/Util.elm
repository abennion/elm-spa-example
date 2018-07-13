module Util exposing (apiUrl, dateStringDecoder, onClickStopPropagation, updateFromResult)

import Html exposing (Attribute, Html)
import Html.Events exposing (stopPropagationOn)
import Iso8601
import Json.Decode as Decode exposing (Decoder, fail, succeed)
import Parser
import Time


apiUrl : String -> String
apiUrl str =
    "https://conduit.productionready.io/api" ++ str


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click"
        (Decode.succeed ( msg, True ))


updateFromResult : { model | errors : List error } -> cmd -> Result error cmd -> ( { model | errors : List error }, cmd )
updateFromResult model fallbackCmd result =
    case result of
        Ok cmd ->
            ( model, cmd )

        Err error ->
            ( { model | errors = model.errors ++ [ error ] }, fallbackCmd )


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
