module Decoders exposing (chordDecoder, chordsDecoder, decode)

import Base64 as Base64
import Json.Decode exposing (Decoder, andThen, decodeString, errorToString, fail, field, float, int, list, map, map4, string, succeed)
import Svg exposing (Svg)
import SvgParser exposing (..)
import Types exposing (..)


decode : Decoder a -> String -> Result String a
decode decoder input =
    case decodeString decoder input of
        Ok decoded ->
            Ok decoded

        Err err ->
            Err (errorToString err)


chordsDecoder : Decoder (List Chord)
chordsDecoder =
    list chordDecoder


chordDecoder : Decoder Chord
chordDecoder =
    map4 Chord idDecoder nameDecoder svgDecoder tagsDecoder


idDecoder : Decoder Int
idDecoder =
    field "id" int


nameDecoder : Decoder String
nameDecoder =
    field "name" string


tagsDecoder : Decoder (List String)
tagsDecoder =
    field "tags" (list string)


svgDecoder : Decoder (Svg Msg)
svgDecoder =
    andThen svgDecoderM (field "svgBase64" string)


svgDecoderM : String -> Decoder (Svg Msg)
svgDecoderM s =
    let
        svgRes =
            stringToSvg s
    in
    case svgRes of
        Ok res ->
            succeed res

        Err err ->
            fail err


stringToSvg : String -> Result String (Svg Msg)
stringToSvg base64 =
    case Base64.decode base64 of
        Ok s ->
            parse s

        Err err ->
            Err err
