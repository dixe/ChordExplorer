module Types exposing (..)

import Html exposing (Html)
import Http exposing (Error)
import Svg exposing (..)
import SvgChord.Types exposing (SvgModel)


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
    | DownloadSvg String
    | UploadChord Chord


type alias Chord =
    { id : Int, name : String, svg : Svg Msg, tags : List String }


type alias Model =
    { chordList : List Chord
    , status : Status
    , svgModel : Maybe SvgModel
    }
