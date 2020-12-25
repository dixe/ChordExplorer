module Pages.CreateChord exposing (Model, Msg, initModel, page, update)

import Api.Api exposing (UploadChord, UploadId, uploadChord)
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Events exposing (..)
import Element.Font
import Element.Input exposing (button)
import File.Download as Download
import Layout.Helper as LH exposing (..)
import Set exposing (Set)
import SvgChord.Logic exposing (initSvgModel, modelClicked, setName)
import SvgChord.Render exposing (createChordView, getSvgString)
import SvgChord.Types exposing (SvgModel)


type Msg
    = SvgClickPos Float Float
    | SvgUpdateName String
    | UploadSvg
    | Uploaded (Result String UploadId)
    | AddTag String
    | UpdateCurrentTag String


type alias Model =
    { svgModel : SvgModel
    , tags : Set String
    , currentTag : Maybe String
    }


tagsRowMaxLen : Int
tagsRowMaxLen =
    300


textInputMaxLen : Int
textInputMaxLen =
    200


initModel : Model
initModel =
    { svgModel = initSvgModel
    , tags = Set.fromList [ "open", "jazzy", "basic", "easy", "beginner", "next", "more new" ] --Set.empty
    , currentTag = Nothing
    }


page : Model -> Element Msg
page model =
    column [ width fill, padding 10 ]
        [ LH.header "Create chord"
        , LH.spacerLine
        , createChordView model.svgModel SvgClickPos
        , viewControls model
        ]


viewControls : Model -> Element Msg
viewControls model =
    let
        r1 =
            Element.row [ LH.defSpacing, width shrink ]
                [ nameControls model
                , uploadControls model
                ]
    in
    Element.column
        [ paddingXY 10 0
        , LH.defSpacing
        ]
        [ r1
        , tagsControls model
        ]


uploadControls : Model -> Element Msg
uploadControls model =
    case model.svgModel.name of
        "" ->
            Element.none

        name ->
            Element.Input.button LH.buttonLayout
                { onPress = Just UploadSvg
                , label = text "UploadSvg"
                }


nameControls : Model -> Element Msg
nameControls model =
    Element.Input.text
        [ width (fill |> maximum textInputMaxLen) ]
        { label = Element.Input.labelHidden "Name"
        , onChange = SvgUpdateName
        , placeholder = Just (Element.Input.placeholder [] (text "Name"))
        , text = model.svgModel.name
        }


tagsControls : Model -> Element Msg
tagsControls model =
    let
        tagInput =
            Element.Input.text
                [ width (fill |> maximum textInputMaxLen) ]
                { label = Element.Input.labelHidden "new Tag"
                , onChange = UpdateCurrentTag
                , placeholder = Just (Element.Input.placeholder [] (text "New tag"))
                , text =
                    case model.currentTag of
                        Nothing ->
                            ""

                        Just tag ->
                            tag
                }

        tagAddButton =
            case model.currentTag of
                Nothing ->
                    Element.none

                Just tag ->
                    Element.Input.button LH.buttonLayout { onPress = Just (AddTag tag), label = text "Add Tag" }

        tagsListView =
            renderTags (Set.toList model.tags)
    in
    Element.column [ LH.defSpacing ]
        [ Element.row [ LH.defSpacing ]
            [ tagInput, tagAddButton ]
        , tagsListView
        ]


renderTags : List String -> Element Msg
renderTags tags =
    Element.wrappedRow [ width (fill |> maximum tagsRowMaxLen), spacing 5 ]
        (List.map renderTag tags)


renderTag : String -> Element Msg
renderTag tag =
    Element.el
        [ Element.Border.rounded 20, Element.Background.color blue, padding 5, Element.Font.size 19 ]
        (text tag)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SvgClickPos x y ->
            ( { model | svgModel = updateSvgModelClick x y model.svgModel }, Cmd.none )

        SvgUpdateName name ->
            ( { model | svgModel = setName name model.svgModel }, Cmd.none )

        UploadSvg ->
            let
                chord =
                    { name = model.svgModel.name, svg = getSvgString model.svgModel True, tags = Set.toList model.tags }
            in
            ( model, uploadChord chord Uploaded )

        Uploaded _ ->
            -- TODO handle upload error and success
            ( model, Cmd.none )

        AddTag tag ->
            ( { model | currentTag = Nothing, tags = Set.insert tag model.tags }, Cmd.none )

        UpdateCurrentTag tag ->
            let
                d =
                    Debug.log "Tag" tag
            in
            ( { model | currentTag = Just tag }, Cmd.none )


updateSvgModelClick : Float -> Float -> SvgModel -> SvgModel
updateSvgModelClick x y model =
    modelClicked { x = x, y = y } model
