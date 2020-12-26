module Api.Api exposing (ApiChord(..), UploadChord, UploadId, loadChords, uploadChord)

import Base64 as Base64
import Http exposing (get)
import Json.Decode exposing (Decoder, andThen, decodeString, errorToString, fail, field, float, int, list, map, map4, string, succeed)
import Json.Encode
import Svg exposing (Svg)
import SvgParser exposing (Element, SvgNode, nodeToSvg, parseToNode)


type ApiChord msg
    = ApiChord { id : Int, name : String, svg : ApiSvg msg, tags : List String }


type alias ApiSvg msg =
    { svg : Svg msg
    , width : Float
    , height : Float
    }


type alias UploadChord =
    { name : String, svg : String, tags : List String }


type alias UploadId =
    { id : Int }


loadChords : List Int -> (Result String (List (ApiChord msg)) -> msg) -> Cmd msg
loadChords ids toMsg =
    let
        queryString =
            String.join "," (List.map String.fromInt ids)

        url =
            "http://localhost:3000/chords?ids=" ++ queryString

        d =
            Debug.log "ApiRquest: " ( url, toMsg )
    in
    Http.get
        { url = url
        , expect = Http.expectString (fromResult toMsg chordsDecoder)
        }


uploadChord : UploadChord -> (Result String UploadId -> msg) -> Cmd msg
uploadChord chord toMsg =
    Http.post
        { url = "http://localhost:3000/chords"
        , body = Http.jsonBody (encodeUploadChord chord)
        , expect = Http.expectString (fromResult toMsg uploadIdDecoder)
        }


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl s ->
            "Bad url : " ++ s

        Http.Timeout ->
            "Http Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus status ->
            "Err status " ++ String.fromInt status

        Http.BadBody s ->
            "Bad body " ++ s



-- TODO maybe return Result String a


fromResult : (Result String a -> msg) -> Decoder a -> Result Http.Error String -> msg
fromResult toMsg decoder result =
    let
        d =
            Debug.log "fromResult: " result
    in
    case result of
        Ok allText ->
            toMsg (decode decoder allText)

        Err errMsg ->
            toMsg (Err (httpErrorToString errMsg))



-- DECODERS


decode : Decoder a -> String -> Result String a
decode decoder input =
    case decodeString decoder input of
        Ok decoded ->
            Ok decoded

        Err err ->
            Err (errorToString err)


chordsDecoder : Decoder (List (ApiChord msg))
chordsDecoder =
    list chordDecoder


chordDecoder : Decoder (ApiChord msg)
chordDecoder =
    map4 createChord idDecoder nameDecoder svgDecoder tagsDecoder


createChord : Int -> String -> SvgNode -> List String -> ApiChord msg
createChord id name svgNode tags =
    ApiChord { id = id, name = name, svg = nodeToApiSvg svgNode, tags = tags }


nodeToApiSvg : SvgNode -> ApiSvg msg
nodeToApiSvg node =
    let
        svg =
            nodeToSvg node

        width =
            getWidth node

        height =
            getHeight node
    in
    { svg = svg
    , height = height
    , width = width
    }


getHeight : SvgNode -> Float
getHeight node =
    case node of
        SvgParser.SvgElement e ->
            getFromAttibs "height" e.attributes

        _ ->
            0


getWidth : SvgNode -> Float
getWidth node =
    case node of
        SvgParser.SvgElement e ->
            getFromAttibs "width" e.attributes

        _ ->
            0


getFromAttibs : String -> List SvgParser.SvgAttribute -> Float
getFromAttibs name attribs =
    let
        attrib =
            List.head (List.filter (\a -> Tuple.first a == name) attribs)

        attribVal =
            case attrib of
                Just a ->
                    String.toFloat (Tuple.second a)

                Nothing ->
                    Nothing
    in
    case attribVal of
        Just a ->
            a

        Nothing ->
            0


idDecoder : Decoder Int
idDecoder =
    field "id" int


nameDecoder : Decoder String
nameDecoder =
    field "name" string


tagsDecoder : Decoder (List String)
tagsDecoder =
    field "tags" (list string)


svgDecoder : Decoder SvgNode
svgDecoder =
    andThen svgDecoderM (field "svgBase64" string)


svgDecoderM : String -> Decoder SvgNode
svgDecoderM s =
    let
        svgRes =
            stringToSvg s
    in
    case svgRes of
        Ok res ->
            succeed res

        Err err ->
            fail ("SvgDecode error " ++ err)


uploadIdDecoder : Decoder UploadId
uploadIdDecoder =
    Json.Decode.map (\id -> { id = id }) idDecoder



-- ENCODERS


encodeUploadChord : UploadChord -> Json.Encode.Value
encodeUploadChord chord =
    Json.Encode.object
        [ ( "name", Json.Encode.string chord.name )
        , ( "svgBase64", Json.Encode.string (svgStringToBase64 chord.svg) )
        , ( "tags", Json.Encode.list Json.Encode.string chord.tags )
        ]



-- SVG FUNCTIONS


svgStringToBase64 : String -> String
svgStringToBase64 inSvg =
    let
        svg =
            -- Svg.String does uses ' and not ", that breaks the Svg.Parser
            String.replace "'" "\"" inSvg
    in
    Base64.encode svg


stringToSvg : String -> Result String SvgNode
stringToSvg base64 =
    let
        decoded =
            Base64.decode base64
    in
    case decoded of
        Ok s ->
            parseToNode s

        Err err ->
            Err ("Base 64 decode error " ++ err)
