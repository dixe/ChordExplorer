module Types exposing (..)

import Html exposing (Html)
import Http exposing (Error)
import Svg exposing (..)


type Status
    = SuccessAll (List Chord)
    | LoadedChords
    | Failure String
    | Loading
    | CreatingChord
    | None


type Msg
    = LoadingChords
    | ChordsLoaded (Result Http.Error String)
    | CreateChord
    | SvgClick Int Int
    | DownloadSvg String


type alias Chord =
    { id : Int, name : String, svg : Svg Msg, tags : List String }


type alias Model =
    { chordList : List Chord, status : Status }
