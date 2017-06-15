module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Navigation
import UrlParser exposing ((</>))


-- MAIN


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { currentLocation : Maybe Route
    , documents : List Document
    , currentDocument : Maybe Document
    , newDocument : Maybe String
    }


type Route
    = HomeRoute
    | DocumentRoute Int


type alias Document =
    { id : Int
    , title : String
    , content : String
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { currentLocation = UrlParser.parsePath route location
      , documents = []
      , currentDocument = Nothing
      , newDocument = Nothing
      }
    , getDocumentsCmd
    )


route : UrlParser.Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map DocumentRoute (UrlParser.s "document" </> UrlParser.int)
        ]



-- UPDATE


type Msg
    = NewUrl String
    | UrlChange Navigation.Location
    | NewDocumentMsg String
    | CreateDocumentMsg
    | CreatedDocumentMsg (Result Http.Error Document)
    | GotDocumentsMsg (Result Http.Error (List Document))
    | ReadDocumentMsg Int
    | GotDocumentMsg (Result Http.Error Document)
    | UpdateDocumentTitleMsg String
    | UpdateDocumentContentMsg String
    | SaveDocumentMsg
    | SavedDocumentMsg (Result Http.Error Document)
    | DeleteDocumentMsg Int
    | DeletedDocumentMsg (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model, Navigation.newUrl url )

        UrlChange location ->
            let
                newLocation =
                    UrlParser.parsePath route location

                command =
                    case newLocation of
                        Just route ->
                            case route of
                                DocumentRoute id ->
                                    getDocumentCmd id

                                HomeRoute ->
                                    Cmd.none

                        Nothing ->
                            Cmd.none
            in
                ( { model | currentLocation = newLocation }, command )

        NewDocumentMsg newDocument ->
            ( { model | newDocument = Just newDocument }, Cmd.none )

        CreateDocumentMsg ->
            case model.newDocument of
                Just newDocument ->
                    ( model, createDocumentCmd newDocument )

                _ ->
                    ( model, Cmd.none )

        CreatedDocumentMsg (Ok document) ->
            ( { model
                | documents = document :: model.documents
                , newDocument = Nothing
                , currentDocument = Just document
                , currentLocation = Just (DocumentRoute document.id)
              }
            , Cmd.none
            )

        CreatedDocumentMsg (Err _) ->
            ( model, Cmd.none )

        GotDocumentsMsg (Ok documents) ->
            ( { model | documents = documents }, Cmd.none )

        GotDocumentsMsg (Err _) ->
            ( model, Cmd.none )

        ReadDocumentMsg id ->
            ( model, getDocumentCmd id )

        GotDocumentMsg (Ok document) ->
            ( { model | currentDocument = Just document }, Cmd.none )

        GotDocumentMsg (Err _) ->
            ( model, Cmd.none )

        UpdateDocumentTitleMsg title ->
            case model.currentDocument of
                Just currentDocument ->
                    let
                        newDocument =
                            { currentDocument | title = title }
                    in
                        ( { model | currentDocument = Just newDocument }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateDocumentContentMsg content ->
            case model.currentDocument of
                Just currentDocument ->
                    let
                        newDocument =
                            { currentDocument | content = content }
                    in
                        ( { model | currentDocument = Just newDocument }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SaveDocumentMsg ->
            case model.currentDocument of
                Just currentDocument ->
                    ( model, saveDocumentCmd currentDocument )

                _ ->
                    ( model, Cmd.none )

        SavedDocumentMsg (Ok _) ->
            ( model, getDocumentsCmd )

        SavedDocumentMsg (Err _) ->
            ( model, Cmd.none )

        DeleteDocumentMsg id ->
            ( model, deleteDocumentCmd <| toString id )

        DeletedDocumentMsg (Ok _) ->
            ( { model | currentLocation = Just HomeRoute }
            , Cmd.batch [ Navigation.newUrl "/", getDocumentsCmd ]
            )

        DeletedDocumentMsg (Err _) ->
            ( model, Cmd.none )


getDocumentsCmd : Cmd Msg
getDocumentsCmd =
    let
        url =
            "http://localhost:3000/documents?_sort=id&_order=desc"

        request =
            Http.get url decodeDocuments
    in
        Http.send GotDocumentsMsg request


getDocumentCmd : Int -> Cmd Msg
getDocumentCmd id =
    let
        url =
            "http://localhost:3000/documents/" ++ toString id

        request =
            Http.get url decodeDocument
    in
        Http.send GotDocumentMsg request


createDocumentCmd : String -> Cmd Msg
createDocumentCmd documentTitle =
    let
        url =
            "http://localhost:3000/documents"

        body =
            Http.jsonBody <| encodeNewDocument documentTitle

        expectedDocument =
            Http.expectJson decodeDocument

        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = url
                , body = body
                , expect = expectedDocument
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send CreatedDocumentMsg request


saveDocumentCmd : Document -> Cmd Msg
saveDocumentCmd document =
    let
        url =
            "http://localhost:3000/documents/" ++ toString document.id

        body =
            Http.jsonBody <| encodeUpdatedDocument document

        expectedDocument =
            Http.expectJson decodeDocument

        request =
            Http.request
                { method = "PUT"
                , headers = []
                , url = url
                , body = body
                , expect = expectedDocument
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send SavedDocumentMsg request


deleteDocumentCmd : String -> Cmd Msg
deleteDocumentCmd id =
    let
        url =
            "http://localhost:3000/documents/" ++ id

        request =
            Http.request
                { method = "DELETE"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send DeletedDocumentMsg request


decodeDocuments : Decode.Decoder (List Document)
decodeDocuments =
    Decode.list decodeDocument


decodeDocument : Decode.Decoder Document
decodeDocument =
    Decode.map3 Document
        (Decode.field "id" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "content" Decode.string)


encodeNewDocument : String -> Encode.Value
encodeNewDocument title =
    let
        object =
            [ ( "title", Encode.string title )
            , ( "content", Encode.string "" )
            ]
    in
        Encode.object object


encodeUpdatedDocument : Document -> Encode.Value
encodeUpdatedDocument document =
    let
        object =
            [ ( "id", Encode.int document.id )
            , ( "title", Encode.string document.title )
            , ( "content", Encode.string document.content )
            ]
    in
        Encode.object object



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ styleMain ]
        [ div [ styleContainer ]
            [ a [ styleHeader, href "/", onClickLink (NewUrl "/") ] [ text "My Personal Notes" ]
            , page model
            ]
        ]


page : Model -> Html Msg
page model =
    case model.currentLocation of
        Just route ->
            case route of
                HomeRoute ->
                    viewHome model

                DocumentRoute id ->
                    case model.currentDocument of
                        Just document ->
                            viewDocument document

                        Nothing ->
                            div [] [ text "Nothing here…" ]

        Nothing ->
            div [] [ text "404 – Not Found" ]


viewHome : Model -> Html Msg
viewHome model =
    div []
        [ Html.form [ onSubmit CreateDocumentMsg ]
            [ input [ styleNewDocument, placeholder "Start a new document…", required True, onInput NewDocumentMsg ] [] ]
        , ul [ styleDocumentList ] (List.map viewDocumentEntry model.documents)
        ]


viewDocumentEntry : Document -> Html Msg
viewDocumentEntry document =
    let
        link =
            "/document/" ++ toString document.id
    in
        li [ styleDocumentEntry ]
            [ a
                [ href link
                , onClickLink (NewUrl link)
                , styleDocumentLink
                ]
                [ text document.title ]
            ]


viewDocument : Document -> Html Msg
viewDocument document =
    article []
        [ input [ styleDocumentTitle, onInput UpdateDocumentTitleMsg, value document.title ] []
        , p [ Html.Attributes.style [ ( "textAlign", "center" ) ] ]
            [ button [ styleButton, onClick SaveDocumentMsg ] [ text "Save" ]
            , button [ styleButton, onClick (DeleteDocumentMsg document.id) ] [ text "Delete" ]
            ]
        , textarea [ styleDocumentContent, onInput UpdateDocumentContentMsg, value document.content ] []
        ]


styleMain : Attribute Msg
styleMain =
    Html.Attributes.style
        [ ( "fontSize", "18px" )
        , ( "fontFamily", "Helvetica Neue" )
        , ( "lineHeight", "1.5" )
        , ( "backgroundColor", "#f3eee9" )
        , ( "minHeight", "100vh" )
        , ( "padding", "2rem 0" )
        ]


styleContainer : Attribute Msg
styleContainer =
    Html.Attributes.style
        [ ( "maxWidth", "1100px" )
        , ( "margin", "0 auto" )
        ]


styleHeader : Attribute Msg
styleHeader =
    Html.Attributes.style
        [ ( "fontSize", "1.2em" )
        , ( "textTransform", "uppercase" )
        , ( "textDecoration", "none" )
        , ( "display", "inline-block" )
        , ( "color", "#db2f27" )
        , ( "fontWeight", "bold" )
        , ( "letterSpacing", "0.05em" )
        , ( "textAlign", "center" )
        , ( "display", "inline-block" )
        , ( "width", "100%" )
        , ( "margin", "1rem 0 2rem" )
        ]


styleNewDocument : Attribute Msg
styleNewDocument =
    Html.Attributes.style
        [ ( "background-color", "transparent" )
        , ( "border", "0" )
        , ( "borderTop", "4px solid #333" )
        , ( "fontSize", "inherit" )
        , ( "marginTop", "1rem" )
        , ( "height", "2rem" )
        , ( "fontWeight", "bold" )
        ]


styleDocumentList : Attribute Msg
styleDocumentList =
    Html.Attributes.style
        [ ( "padding", "0" )
        , ( "margin", "0" )
        ]


styleDocumentEntry : Attribute Msg
styleDocumentEntry =
    Html.Attributes.style
        [ ( "borderTop", "2px solid #555" )
        , ( "listStyle", "none" )
        ]


styleDocumentLink : Attribute Msg
styleDocumentLink =
    Html.Attributes.style
        [ ( "textDecoration", "none" )
        , ( "color", "#333333" )
        , ( "lineHeight", "2em" )
        ]


styleDocumentTitle : Attribute Msg
styleDocumentTitle =
    Html.Attributes.style
        [ ( "font-size", "2.5em" )
        , ( "textAlign", "center" )
        , ( "backgroundColor", "transparent" )
        , ( "border", "0" )
        ]


styleDocumentContent : Attribute Msg
styleDocumentContent =
    Html.Attributes.style
        [ ( "minHeight", "100vh" )
        , ( "backgroundColor", "transparent" )
        , ( "border", "0" )
        , ( "fontSize", "18px" )
        , ( "margin", "3rem auto 0" )
        , ( "lineHeight", "inherit" )
        , ( "display", "block" )
        , ( "maxWidth", "35em" )
        ]


styleButton : Attribute Msg
styleButton =
    Html.Attributes.style
        [ ( "margin", "0 0.5em" )
        , ( "backgroundColor", "transparent" )
        , ( "border", "0" )
        , ( "fontSize", "16px" )
        , ( "textTransform", "uppercase" )
        , ( "letterSpacing", "0.05em" )
        , ( "fontWeight", "bold" )
        , ( "color", "#999" )
        , ( "cursor", "pointer" )
        ]


onClickLink : msg -> Attribute msg
onClickLink message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions "click" options (Decode.succeed message)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
