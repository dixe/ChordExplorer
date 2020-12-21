module ChordsOverView exposing (Model, Msg, page)


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


browseChords : Cmd Msg
browseChords =
    Http.get
        { url = "http://localhost:3000/chords"
        , expect = Http.expectString ChordsLoaded
        }


type Status
    = SuccessAll (List Chord)
    | LoadedChords
    | Failure String
    | Loading
    | CreatingChord
    | None


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadingChords ->
            ( model, browseChords )

        CreateChord ->
            ( { model | chordList = [], status = CreatingChord }, Cmd.none )

        SvgClickPos x y ->
            ( { model | svgModel = updateSvgModelClick x y model.svgModel }, Cmd.none )

        ChordsLoaded res ->
            loadedChords model res

        DownloadSvg s ->
            ( model, saveSvg s )

        UploadChord c ->
            ( model, Cmd.none )
