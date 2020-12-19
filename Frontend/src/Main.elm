module Main exposing (..)

import Types exposing (..)
import Html exposing (..)
import Layout exposing ( viewMain)
import Browser
import Task

import Http
import Json.Decode exposing (Decoder, field, map3, int, string, decodeString, errorToString, list)
import Decoders exposing (..)

-- MAIN
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

init : () -> (Model, Cmd Msg)
init _ = ({chordList = [], status = None}, browseChords)

-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadingChords -> (model, browseChords)
        ChordsLoaded res ->
            let
                (status, cmd) = fromResult res chordsDecoder SuccessAll
            in
                case status of
                    SuccessAll chords -> ({chordList = chords, status = LoadedChords}, cmd)
                    Failure reason -> ({chordList = model.chordList, status = Failure reason}, Cmd.none)
                    None -> ({chordList = model.chordList, status = None}, Cmd.none)
                    Loading -> ({chordList = model.chordList, status = Loading}, Cmd.none)
                    LoadedChords -> ({chordList = model.chordList, status = LoadedChords}, Cmd.none)

browseChords : (Cmd Msg)
browseChords = Http.get { url = "http://localhost:3000/chords"
                        , expect = Http.expectString ChordsLoaded
                        }




fromResult : (Result Http.Error String) -> Decoder a -> (a -> Status) -> (Status, Cmd Msg)
fromResult result decoder ret  =
    case result of
        Ok allText ->
            case decode decoder allText of
                Ok res ->
                    (ret res, Cmd.none)
                Err err ->
                    (Failure err, Cmd.none)
        Err _ ->
            (Failure "http error", Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = viewMain model



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =  Sub.none
