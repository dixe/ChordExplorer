module Main exposing (..)

import Browser
import Bytes exposing (Bytes)
import Decoders exposing (..)
import File.Download as Download
import Html exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeString, errorToString, field, int, list, map3, string)
import Layout exposing (viewMain)
import Task
import Types exposing (..)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { chordList = [], status = None }, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadingChords ->
            ( model, browseChords )

        CreateChord ->
            ( { chordList = [], status = CreatingChord }, Cmd.none )

        SvgClick x y ->
            ( model, Cmd.none )

        ChordsLoaded res ->
            loadedChords model res

        DownloadSvg s ->
            ( model, saveSvg s )


saveSvg : String -> Cmd Msg
saveSvg content =
    Download.string "chord.svg" "application/svg" content


loadedChords : Model -> Result Http.Error String -> ( Model, Cmd Msg )
loadedChords model res =
    let
        ( status, cmd ) =
            fromResult res chordsDecoder SuccessAll
    in
    case status of
        SuccessAll chords ->
            ( { chordList = chords, status = LoadedChords }, cmd )

        Failure reason ->
            ( { chordList = model.chordList, status = Failure reason }, Cmd.none )

        CreatingChord ->
            ( model, Cmd.none )

        None ->
            ( { chordList = model.chordList, status = None }, Cmd.none )

        Loading ->
            ( { chordList = model.chordList, status = Loading }, Cmd.none )

        LoadedChords ->
            ( { chordList = model.chordList, status = LoadedChords }, Cmd.none )


browseChords : Cmd Msg
browseChords =
    Http.get
        { url = "http://localhost:3000/chords"
        , expect = Http.expectString ChordsLoaded
        }


fromResult : Result Http.Error String -> Decoder a -> (a -> Status) -> ( Status, Cmd Msg )
fromResult result decoder ret =
    case result of
        Ok allText ->
            case decode decoder allText of
                Ok res ->
                    ( ret res, Cmd.none )

                Err err ->
                    ( Failure err, Cmd.none )

        Err _ ->
            ( Failure "http error", Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    viewMain model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
