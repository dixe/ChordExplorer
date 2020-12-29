module Routing exposing (Model, Msg, init, onUrlChange, onUrlRequest, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element)
import Html exposing (Html)
import Pages.ChordsOverview as ChordsOverview
import Pages.CreateChord as CreateChord
import Pages.PlayAlong as PlayAlong
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, s, string)
import Url.Parser.Query as Query



--PAGES


type alias Model =
    { key : Nav.Key, page : Maybe Page, inner : InnerModel }


type InnerModel
    = CreateChord_Model CreateChord.Model
    | ChordsOverview_Model ChordsOverview.Model
    | PlayAlong_Model PlayAlong.Model
    | NotFound


type Msg
    = CreateChord_Msg CreateChord.Msg
    | ChordsOverview_Msg ChordsOverview.Msg
    | PlayAlong_Msg PlayAlong.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


type Route
    = PlayALong (List (Maybe Int))
    | CreateChord
    | ChordsOverview


type alias Page =
    { title : String
    , modelIsFun : InnerModel -> Bool
    , toHtmlFun : InnerModel -> List (Html Msg)
    , model : PageModel
    , parseMsg : Msg -> InnerModel -> ( InnerModel, Cmd Msg )
    , initMsg : Cmd Msg
    , getSubscriptions : InnerModel -> Sub Msg
    }


type PageModel
    = WithContext (Model -> InnerModel)
    | NoContext InnerModel



-- ROUTE PARSING


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map PlayALong (s "playAlong" <?> idsParser)
        , map CreateChord (s "createChord")
        , map ChordsOverview (s "overview")
        ]


routeToPage : Route -> Page
routeToPage r =
    case r of
        PlayALong ids ->
            { title = "PlayAlong"
            , modelIsFun = isPlayAlong
            , toHtmlFun = playAlongPage
            , model = WithContext (initialzieIsPlayModel (stripMaybe ids))
            , parseMsg = parseMsgPlayAlong
            , initMsg = playAlongInitMsg
            , getSubscriptions = playAlongSubScriptions
            }

        ChordsOverview ->
            { title = "Overview"
            , modelIsFun = isChordOverview
            , toHtmlFun = chordsOverviewPage
            , model = NoContext (ChordsOverview_Model ChordsOverview.initModel)
            , parseMsg = parseMsgOverview
            , initMsg = chordsOverviewInitMsg
            , getSubscriptions = \_ -> Sub.none
            }

        CreateChord ->
            { title = "Create Chord"
            , modelIsFun = isCreateChord
            , toHtmlFun = createChordPage
            , model = NoContext (CreateChord_Model CreateChord.initModel)
            , parseMsg = parseMsgCreateChord
            , initMsg = Cmd.none
            , getSubscriptions = \_ -> Sub.none
            }


stripMaybe : List (Maybe a) -> List a
stripMaybe l =
    case l of
        [] ->
            []

        c :: cs ->
            case c of
                Nothing ->
                    stripMaybe cs

                Just b ->
                    b :: stripMaybe cs


idsParser : Query.Parser (List (Maybe Int))
idsParser =
    Query.custom "ids" (List.map String.toInt)



-- PLAYALONG


playAlongSubScriptions : InnerModel -> Sub Msg
playAlongSubScriptions model =
    case model of
        PlayAlong_Model m ->
            Sub.map PlayAlong_Msg (PlayAlong.subscriptions m)

        _ ->
            Sub.none


playAlongInitMsg : Cmd Msg
playAlongInitMsg =
    Cmd.map PlayAlong_Msg PlayAlong.initMsg


initialzieIsPlayModel : List Int -> Model -> InnerModel
initialzieIsPlayModel ids model =
    let
        chords =
            case model.inner of
                ChordsOverview_Model result ->
                    case result of
                        Ok overviewChords ->
                            List.map mapChord (List.filter (\c -> c.selected) overviewChords)

                        Err _ ->
                            []

                _ ->
                    []
    in
    PlayAlong_Model (PlayAlong.initModel ids (List.map PlayAlong.toChord chords))


mapChord : ChordsOverview.Chord -> PlayAlong.ChordBase ChordsOverview.Msg
mapChord chord =
    { id = chord.id, name = chord.name, svg = chord.svg, svgHeight = chord.svgHeight, svgWidth = chord.svgWidth }


isPlayAlong : InnerModel -> Bool
isPlayAlong model =
    case model of
        PlayAlong_Model m ->
            True

        _ ->
            False


playAlongPage : InnerModel -> List (Html Msg)
playAlongPage model =
    case model of
        PlayAlong_Model m ->
            [ Element.layout [] (Element.map PlayAlong_Msg (PlayAlong.page m)) ]

        _ ->
            []


parseMsgPlayAlong : Msg -> InnerModel -> ( InnerModel, Cmd Msg )
parseMsgPlayAlong msg model =
    case ( msg, model ) of
        ( PlayAlong_Msg cMsg, PlayAlong_Model m ) ->
            updateInner PlayAlong.update PlayAlong_Model PlayAlong_Msg cMsg m

        _ ->
            ( model, Cmd.none )



-- CREATE CHORDS


createChordPage : InnerModel -> List (Html Msg)
createChordPage model =
    case model of
        CreateChord_Model m ->
            [ Element.layout [] (Element.map CreateChord_Msg (CreateChord.page m)) ]

        _ ->
            []


parseMsgCreateChord : Msg -> InnerModel -> ( InnerModel, Cmd Msg )
parseMsgCreateChord msg model =
    case ( msg, model ) of
        ( CreateChord_Msg cMsg, CreateChord_Model m ) ->
            updateInner CreateChord.update CreateChord_Model CreateChord_Msg cMsg m

        _ ->
            ( model, Cmd.none )


isCreateChord : InnerModel -> Bool
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


chordsOverviewPage : InnerModel -> List (Html Msg)
chordsOverviewPage model =
    case model of
        ChordsOverview_Model m ->
            [ Element.layout [] (Element.map ChordsOverview_Msg (ChordsOverview.page m)) ]

        _ ->
            []


parseMsgOverview : Msg -> InnerModel -> ( InnerModel, Cmd Msg )
parseMsgOverview msg model =
    case ( msg, model ) of
        ( ChordsOverview_Msg cMsg, ChordsOverview_Model m ) ->
            updateInner ChordsOverview.update ChordsOverview_Model ChordsOverview_Msg cMsg m

        _ ->
            ( model, Cmd.none )


isChordOverview : InnerModel -> Bool
isChordOverview model =
    case model of
        ChordsOverview_Model m ->
            True

        _ ->
            False



-- PAGE UPDATE HELPER


updateInner : (msg -> model -> ( model, Cmd msg )) -> (model -> InnerModel) -> (msg -> Msg) -> msg -> model -> ( InnerModel, Cmd Msg )
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
            model.page
    in
    fromPage model.inner r


fromPage : InnerModel -> Maybe Page -> Browser.Document Msg
fromPage model page =
    case page of
        Just r ->
            Browser.Document r.title (pageToHtml model r)

        Nothing ->
            Browser.Document "Redirect" []


pageToHtml : InnerModel -> Page -> List (Html Msg)
pageToHtml model r =
    r.toHtmlFun model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            routing url model

        _ ->
            case model.page of
                Just p ->
                    let
                        ( inner, nMsg ) =
                            p.parseMsg msg model.inner
                    in
                    ( { model | inner = inner }, nMsg )

                Nothing ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Just p ->
            p.getSubscriptions model.inner

        Nothing ->
            Sub.none


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    routing url { key = key, page = Nothing, inner = NotFound }


routing : Url -> Model -> ( Model, Cmd Msg )
routing url model =
    let
        route : Maybe Route
        route =
            Url.Parser.parse routeParser url

        page =
            Maybe.map routeToPage route

        d =
            Debug.log "Url " url

        d1 =
            Debug.log "route:  " route
    in
    case page of
        Nothing ->
            ( { inner = NotFound, page = Nothing, key = model.key }, Nav.load "/createChord" )

        Just p ->
            ( { key = model.key, page = page, inner = getPageInnerModel model p }, p.initMsg )


getPageInnerModel : Model -> Page -> InnerModel
getPageInnerModel model page =
    case page.model of
        NoContext m ->
            m

        WithContext f ->
            f model


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlReq =
    let
        d =
            Debug.log "UrlRequest" urlReq
    in
    LinkClicked urlReq


onUrlChange : Url.Url -> Msg
onUrlChange url =
    let
        d =
            Debug.log "UrlChange" url
    in
    UrlChanged url
