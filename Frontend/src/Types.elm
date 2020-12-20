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
    { x : Float
    , y : Float
    , lineWidth : Float
    , width : Float
    , height : Float
    , stringSpace : Float
    , fretSpace : Float
    , numStrings : Int
    , numFrets : Int
    , diameter : Float
    }


type alias Pos =
    { x : Float, y : Float }


type alias FretNum =
    Int


type StringNum
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type Fret
    = Open
    | Mute
    | Fret FretNum


type FretCandidate
    = FretCandidate StringNum FretNum


type alias Frets =
    { string1 : Fret
    , string2 : Fret
    , string3 : Fret
    , string5 : Fret
    , string4 : Fret
    , string6 : Fret
    }


type alias SvgModel =
    { info : ImgInfo, clickPos : Maybe Pos, frets : Frets }
