module Pages.CreateChord exposing (Model, Msg, initModel, page, update)

import Api.Api exposing (UploadChord, UploadId, uploadChord)
import Element exposing (..)
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import File.Download as Download
import Layout.Helper as LH exposing (..)
import SvgChord.Logic exposing (initSvgModel, modelClicked)
import SvgChord.Render exposing (createChordView, getSvgString)
import SvgChord.Types exposing (SvgModel)


type Msg
    = SvgClickPos Float Float
    | SvgUpdateName String
    | UploadSvg
    | Uploaded (Result String UploadId)


type alias Model =
    SvgModel


initModel : Model
initModel =
    initSvgModel


page : Model -> Element Msg
page model =
    column [ width fill, spacing 10 ]
        [ LH.header
        , LH.spacerLine
        , createChordView model SvgClickPos
        , viewControls model
        ]


viewControls : Model -> Element Msg
viewControls model =
    Element.row []
        [ Element.Input.text [ width shrink ]
            { label = Element.Input.labelHidden "Name"
            , onChange = SvgUpdateName
            , placeholder = Just (Element.Input.placeholder [] (text "Name"))
            , text = model.name
            }
        , Element.Input.button []
            { onPress = Just UploadSvg
            , label = text "UploadSvg"
            }
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SvgClickPos x y ->
            ( updateSvgModelClick x y model, Cmd.none )

        SvgUpdateName name ->
            ( { model | name = name }, Cmd.none )

        UploadSvg ->
            let
                chord =
                    { name = model.name, svg = getSvgString model True, tags = [] }
            in
            ( model, uploadChord chord Uploaded )

        Uploaded _ ->
            -- TODO handle upload error and success
            ( model, Cmd.none )



-- TODO hande


updateSvgModelClick : Float -> Float -> SvgModel -> SvgModel
updateSvgModelClick x y model =
    modelClicked { x = x, y = y } model
