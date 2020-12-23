module Routing exposing (Model, Msg, init, onUrlChange, onUrlRequest, update, view)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, layout, map)
import Html exposing (Html)
import Pages.ChordsOverview as ChordsOverview
import Pages.CreateChord as CreateChord
import Url exposing (Url)



--PAGES


type Model
    = CreateChord_Model CreateChord.Model
    | ChordsOverview_Model ChordsOverview.Model
    | NotFound


type Msg
    = CreateChord_Msg CreateChord.Msg
    | ChordsOverview_Msg ChordsOverview.Msg
    | UrlRequest
    | UrlChange


type alias Path =
    String


type alias Page =
    { path : Path
    , title : String
    , modelIsFun : Model -> Bool
    , toHtmlFun : Model -> List (Html Msg)
    , model : Model
    , parseMsg : Msg -> Model -> ( Model, Cmd Msg )
    , initMsg : Cmd Msg
    }


allPages : List Page
allPages =
    [ Page "/createChord" "Create Chord" isCreateChord createChordPage (CreateChord_Model CreateChord.initModel) parseMsgCreateChord Cmd.none
    , Page "/chordsOverview" "Overview" isChordOverview chordsOverviewPage (ChordsOverview_Model ChordsOverview.initModel) parseMsgOverview chordsOverviewInitMsg
    ]



-- CREATE CHORDS


createChordPage : Model -> List (Html Msg)
createChordPage model =
    case model of
        CreateChord_Model m ->
            [ layout [] (map CreateChord_Msg (CreateChord.page m)) ]

        _ ->
            []


parseMsgCreateChord : Msg -> Model -> ( Model, Cmd Msg )
parseMsgCreateChord msg model =
    case ( msg, model ) of
        ( CreateChord_Msg cMsg, CreateChord_Model m ) ->
            updateInner CreateChord.update CreateChord_Model CreateChord_Msg cMsg m

        _ ->
            ( model, Cmd.none )


isCreateChord : Model -> Bool
isCreateChord model =
    case model of
        CreateChord_Model m ->
            True

        _ ->
            False



-- CHORDS OVERVIEW


chordsOverviewInitMsg : Cmd Msg
chordsOverviewInitMsg =
    Cmd.map ChordsOverview_Msg ChordsOverview.initMsg


chordsOverviewPage : Model -> List (Html Msg)
chordsOverviewPage model =
    case model of
        ChordsOverview_Model m ->
            [ layout [] (map ChordsOverview_Msg (ChordsOverview.page m)) ]

        _ ->
            []


parseMsgOverview : Msg -> Model -> ( Model, Cmd Msg )
parseMsgOverview msg model =
    case ( msg, model ) of
        ( ChordsOverview_Msg cMsg, ChordsOverview_Model m ) ->
            updateInner ChordsOverview.update ChordsOverview_Model ChordsOverview_Msg cMsg m

        _ ->
            ( model, Cmd.none )


isChordOverview : Model -> Bool
isChordOverview model =
    case model of
        ChordsOverview_Model m ->
            True

        _ ->
            False



-- PAGE UPDATE HELPER


updateInner : (msg -> model -> ( model, Cmd msg )) -> (model -> Model) -> (msg -> Msg) -> msg -> model -> ( Model, Cmd Msg )
updateInner updateFun toModel mapMsg msg model =
    let
        ( updatedModel, innerMsg ) =
            updateFun msg model
    in
    ( toModel updatedModel, Cmd.map mapMsg innerMsg )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        r =
            findPageFromModel model allPages
    in
    fromPage model r


fromPage : Model -> Maybe Page -> Browser.Document Msg
fromPage model page =
    case page of
        Just r ->
            Browser.Document r.title (pageToHtml model r)

        Nothing ->
            Browser.Document "Redirect" []


pageToHtml : Model -> Page -> List (Html Msg)
pageToHtml model r =
    r.toHtmlFun model


findPageFromModel : Model -> List Page -> Maybe Page
findPageFromModel model pages =
    first isPage model pages


isPage : Model -> Page -> Bool
isPage model page =
    page.modelIsFun model


first : (a -> b -> Bool) -> a -> List b -> Maybe b
first isFun target inputs =
    case inputs of
        [] ->
            Nothing

        h :: t ->
            if isFun target h then
                Just h

            else
                first isFun target t


isPath : String -> Page -> Bool
isPath path page =
    path == page.path


pageModel : Page -> Model
pageModel page =
    page.model


pageMsg : Page -> Cmd Msg
pageMsg page =
    let
        d =
            Debug.log "PageMsg" page
    in
    page.initMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        page =
            first isPage model allPages
    in
    case page of
        Just p ->
            p.parseMsg msg model

        Nothing ->
            ( model, Cmd.none )


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    routing url.path allPages


routing : String -> List Page -> ( Model, Cmd Msg )
routing path pages =
    let
        page =
            first isPath path pages

        d =
            Debug.log "Routing" path
    in
    case page of
        Just p ->
            ( pageModel p, pageMsg p )

        _ ->
            ( NotFound, Nav.load "/createChord" )


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlReq =
    let
        a =
            Debug.log "onUrlRequests" urlReq
    in
    UrlRequest


onUrlChange : Url.Url -> Msg
onUrlChange url =
    let
        a =
            Debug.log "onChange" url
    in
    UrlChange
