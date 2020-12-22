module Api.Api exposing (Chord(..), loadChords)

import Base64 as Base64
import Http exposing (get)
import Json.Decode exposing (Decoder, andThen, decodeString, errorToString, fail, field, float, int, list, map, map4, string, succeed)
import Svg exposing (Svg)
import SvgParser exposing (parse)


type Chord msg
    = Chord { id : Int, name : String, svg : Svg msg, tags : List String }


loadChords : (Result String (List (Chord msg)) -> msg) -> Cmd msg
loadChords toMsg =
    Http.get
        { url = "http://localhost:3000/chords"
        , expect = Http.expectString (fromResult toMsg chordsDecoder)
        }



-- TODO maybe return Result String a


fromResult : (Result String (List (Chord msg)) -> msg) -> Decoder (List (Chord msg)) -> Result Http.Error String -> msg
fromResult toMsg decoder result =
    case result of
        Ok allText ->
            toMsg (decode decoder allText)

        Err _ ->
            toMsg (Err "")


decode : Decoder a -> String -> Result String a
decode decoder input =
    case decodeString decoder input of
        Ok decoded ->
            Ok decoded

        Err err ->
            Err (errorToString err)


chordsDecoder : Decoder (List (Chord msg))
chordsDecoder =
    list chordDecoder


chordDecoder : Decoder (Chord msg)
chordDecoder =
    map4 createChord idDecoder nameDecoder svgDecoder tagsDecoder


createChord : Int -> String -> Svg msg -> List String -> Chord msg
createChord id name svg tags =
    Chord { id = id, name = name, svg = svg, tags = tags }


idDecoder : Decoder Int
idDecoder =
    field "id" int


nameDecoder : Decoder String
nameDecoder =
    field "name" string


tagsDecoder : Decoder (List String)
tagsDecoder =
    field "tags" (list string)


svgDecoder : Decoder (Svg msg)
svgDecoder =
    andThen svgDecoderM (field "svgBase64" string)


svgDecoderM : String -> Decoder (Svg msg)
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


stringToSvg : String -> Result String (Svg msg)
stringToSvg base64 =
    case Base64.decode base64 of
        Ok s ->
            parse s

        Err err ->
            Err err
