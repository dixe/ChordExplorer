module SvgChordLogic exposing (getFretsPos, initSvgModel, modelClicked)

import Types exposing (..)


modelClicked : Pos -> SvgModel -> SvgModel
modelClicked pos model =
    let
        info =
            model.info
    in
    { model | frets = updateFrets model.frets info pos }


getClosestStringNum : ImgInfo -> Float -> Int
getClosestStringNum info x =
    let
        string =
            (x - info.x) / info.stringSpace
    in
    round string


getClosestFretNum : ImgInfo -> Float -> Int
getClosestFretNum info y =
    let
        fret =
            (y - info.y + (info.fretSpace / 2)) / info.fretSpace
    in
    round fret


initSvgModel : SvgModel
initSvgModel =
    { clickPos = Nothing, info = createDefaultImgInfo, frets = initFrets }


initFrets : Frets
initFrets =
    { string1 = Open
    , string2 = Open
    , string3 = Open
    , string4 = Open
    , string5 = Open
    , string6 = Open
    }


createDefaultImgInfo : ImgInfo
createDefaultImgInfo =
    let
        stringSpace =
            40

        fretSpace =
            70

        numStrings =
            6

        numFrets =
            4

        imgHeight =
            fretSpace * numFrets

        imgWidth =
            stringSpace * (numStrings - 1)
    in
    { x = 60
    , y = 60
    , lineWidth = 4
    , diameter = 9
    , width = imgWidth
    , height = imgHeight
    , stringSpace = stringSpace
    , fretSpace = fretSpace
    , numStrings = numStrings
    , numFrets = numFrets
    }


updateFrets : Frets -> ImgInfo -> Pos -> Frets
updateFrets frets info pos =
    let
        debug =
            Debug.log "Frets" frets
    in
    case getMaybeFretCandidate info pos of
        Nothing ->
            frets

        Just candidate ->
            case getFretString candidate of
                One ->
                    { frets | string1 = updateFret info frets.string1 candidate }

                Two ->
                    { frets | string2 = updateFret info frets.string2 candidate }

                Three ->
                    { frets | string3 = updateFret info frets.string3 candidate }

                Four ->
                    { frets | string4 = updateFret info frets.string4 candidate }

                Five ->
                    { frets | string5 = updateFret info frets.string5 candidate }

                Six ->
                    { frets | string6 = updateFret info frets.string6 candidate }


getMaybeFretCandidate : ImgInfo -> Pos -> Maybe FretCandidate
getMaybeFretCandidate info pos =
    let
        stringNum =
            getStringNum (getClosestStringNum info pos.x)

        fretNum =
            getClosestFretNum info pos.y

        a =
            Debug.log "stringNum " stringNum

        b =
            Debug.log "fretNum " fretNum
    in
    if validFret info fretNum then
        Nothing

    else
        case stringNum of
            Nothing ->
                Nothing

            Just sn ->
                Just (FretCandidate sn fretNum)


getStringNum : Int -> Maybe StringNum
getStringNum stringNum =
    case stringNum of
        0 ->
            Just One

        1 ->
            Just Two

        2 ->
            Just Three

        3 ->
            Just Four

        4 ->
            Just Five

        5 ->
            Just Six

        _ ->
            Nothing


updateFret : ImgInfo -> Fret -> FretCandidate -> Fret
updateFret info old (FretCandidate sn fn) =
    let
        new =
            case fn of
                0 ->
                    Open

                a ->
                    Fret a
    in
    if old == new then
        case new of
            Open ->
                Mute

            _ ->
                Open

    else
        new


validFret : ImgInfo -> Int -> Bool
validFret info fretNum =
    fretNum >= 0 && fretNum <= info.numFrets


getFretString : FretCandidate -> StringNum
getFretString (FretCandidate sn _) =
    sn


getFretsPos : ImgInfo -> Frets -> List ( Fret, Pos )
getFretsPos info frets =
    [ getFretPos info frets.string1 0
    , getFretPos info frets.string2 1
    , getFretPos info frets.string3 2
    , getFretPos info frets.string4 3
    , getFretPos info frets.string5 4
    , getFretPos info frets.string6 5
    ]


getFretPos : ImgInfo -> Fret -> Int -> ( Fret, Pos )
getFretPos info fret sn =
    let
        stringNum =
            toFloat sn

        fretNum =
            case fret of
                Open ->
                    0

                Mute ->
                    0

                Fret fn ->
                    toFloat fn

        pos =
            { x = info.x + stringNum * info.stringSpace + info.lineWidth / 2, y = info.y + fretNum * info.fretSpace - info.fretSpace / 2 }
    in
    ( fret, pos )
