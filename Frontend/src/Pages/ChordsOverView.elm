module Pages.ChordsOverview exposing (Model, Msg, initModel, page, update)

import Api.Api exposing (Chord(..), loadChords)
import Element exposing (..)
import Layout.Helper as LH exposing (..)



--TYPES


type alias Model =
    { chordList : List (Chord Msg)
    }


type Msg
    = LoadChords
    | ChordsLoaded (Result String (List (Chord Msg)))


initModel : Model
initModel =
    { chordList = [] }


page : Model -> Element Msg
page model =
    column [ width fill, spacing 10 ]
        [ LH.header
        , LH.spacerLine
        ]



{-

   loadedChords : Model -> Result Http.Error String -> ( Model, Cmd Msg )
   loadedChords model res =
       let
           ( status, cmd ) =
               fromResult res chordsDecoder SuccessAll
       in
       case status of
           SuccessAll chords ->
               ( { model | chordList = chords, status = LoadedChords }, cmd )

           Failure reason ->
               ( { model | chordList = model.chordList, status = Failure reason }, Cmd.none )

           CreatingChord ->
               ( model, Cmd.none )

           None ->
               ( { model | chordList = model.chordList, status = None }, Cmd.none )

           Loading ->
               ( { model | chordList = model.chordList, status = Loading }, Cmd.none )

           LoadedChords ->
               ( { model | chordList = model.chordList, status = LoadedChords }, Cmd.none )
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadChords ->
            ( model, loadChords ChordsLoaded )

        ChordsLoaded res ->
            case res of
                Ok chords ->
                    ( { chordList = chords }, Cmd.none )

                Err _ ->
                    ( { chordList = [] }, Cmd.none )



{-
   ChordsLoaded res ->
       loadedChords model res

-}
