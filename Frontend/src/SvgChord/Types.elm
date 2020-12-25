module SvgChord.Types exposing (..)


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
    , radius : Float
    , imgHeight : Float
    , imgWidth : Float
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
    { info : ImgInfo, clickPos : Maybe Pos, frets : Frets, name : String }
