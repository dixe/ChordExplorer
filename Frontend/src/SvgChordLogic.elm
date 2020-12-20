module SvgChordLogic exposing (initSvgModel)

import Types exposing (..)


initSvgModel : SvgModel
initSvgModel =
    { clickPos = Nothing, info = createDefaultImgInfo }


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
    { x = 30
    , y = 30
    , lineWidth = 4
    , width = imgWidth
    , height = imgHeight
    , stringSpace = stringSpace
    , fretSpace = fretSpace
    , numStrings = numStrings
    , numFrets = numFrets
    }
