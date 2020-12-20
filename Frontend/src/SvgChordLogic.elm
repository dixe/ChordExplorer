module SvgChordLogic exposing (getFretPos, getFretType, initSvgModel, modelClicked)

import Types exposing (..)


modelClicked : Pos -> SvgModel -> SvgModel
modelClicked pos model =
    let
        info =
            model.info
    in
    { model | frets = updateFrets model.frets info (Fret (getClosestStringNum info pos.x) (getClosestFretNum info pos.y) Play) }


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
    { clickPos = Nothing, info = createDefaultImgInfo, frets = [] }


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
    { x = 0
    , y = 0
    , lineWidth = 4
    , diameter = 9
    , width = imgWidth
    , height = imgHeight
    , stringSpace = stringSpace
    , fretSpace = fretSpace
    , numStrings = numStrings
    , numFrets = numFrets
    }


getFretType : Fret -> FretType
getFretType (Fret _ _ t) =
    t


withType : Fret -> FretType -> Fret
withType (Fret sn fn _) t =
    Fret sn fn t


updateFrets : List Fret -> ImgInfo -> Fret -> List Fret
updateFrets frets info fret =
    if not (validFret info fret) then
        frets

    else
        let
            existing =
                pop (\f -> isEqualFret fret f)
                    frets
        in
        case existing of
            Nothing ->
                frets ++ [ fret ]

            Just ex ->
                removeAndUpdateFret frets fret ex


removeAndUpdateFret : List Fret -> Fret -> Fret -> List Fret
removeAndUpdateFret frets fret existing =
    let
        without =
            List.filter (\f -> not (isEqualFret fret f)) frets

        res =
            case getFretType existing of
                Play ->
                    [ withType fret Mute ]

                Mute ->
                    []
    in
    without ++ res


validFret : ImgInfo -> Fret -> Bool
validFret info (Fret stringNum fretNum _) =
    fretNum >= 0 && fretNum <= info.numFrets && stringNum >= 0 && stringNum < info.numStrings


pop : (a -> Bool) -> List a -> Maybe a
pop fun l =
    case l of
        [] ->
            Nothing

        element :: elements ->
            case fun element of
                True ->
                    Just element

                False ->
                    pop fun elements


isEqualFret : Fret -> Fret -> Bool
isEqualFret (Fret s1 f1 _) (Fret s2 f2 _) =
    s1 == s2 && f1 == f2


getFretPos : ImgInfo -> Fret -> Pos
getFretPos info (Fret sn fn _) =
    let
        stringNum =
            toFloat sn

        fretNum =
            toFloat fn
    in
    { x = info.x + stringNum * info.stringSpace + info.lineWidth / 2, y = info.y + fretNum * info.fretSpace - info.fretSpace / 2 }
