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
    | DownloadSvg String
    | SvgClickPos Float Float


type alias Chord =
    { id : Int, name : String, svg : Svg Msg, tags : List String }


type alias Model =
    { chordList : List Chord
    , status : Status
    , svgModel : Maybe SvgModel
    }



-- SVG CREATE MODEL


type alias ImgInfo =
    { x : Int, y : Int, lineWidth : Int, width : Int, height : Int, stringSpace : Int, fretSpace : Int, numStrings : Int, numFrets : Int }


type alias Pos =
    { x : Float, y : Float }


type alias SvgModel =
    { info : ImgInfo, clickPos : Maybe Pos }
