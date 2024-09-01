port module Frontend exposing (addEvent, app)

import Array exposing (Array)
import AssocSet
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Dict
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Html
import Html.Attributes
import Icons
import Json.Decode
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Maybe.Extra
import Random
import Sha256
import Task
import Types exposing (..)
import Ui
import Ui.Anim
import Ui.Events
import Ui.Font
import Ui.Input
import Ui.Prose
import Url
import Url.Parser


port copy_to_clipboard_to_js : String -> Cmd msg


port write_file_to_js : String -> Cmd msg


port select_file_to_js : () -> Cmd msg


port select_file_from_js : ({ name : String, content : String } -> msg) -> Sub msg


port got_file_api_not_supported : (() -> msg) -> Sub msg


port get_file_api_not_supported : () -> Cmd msg


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
                    , select_file_from_js GotFile
                    , got_file_api_not_supported (\() -> FileApiNotSupportedFromPort)
                    ]
        , view = view
        }


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    case Url.Parser.parse Url.Parser.string url of
        Just sessionName ->
            ( LoadingSession
                { key = key
                , sessionName = SessionName sessionName
                }
            , Cmd.batch
                [ get_file_api_not_supported ()
                , Lamdera.sendToBackend (LoadSessionRequest (SessionName sessionName))
                ]
            )

        Nothing ->
            ( LoadingSession { key = key, sessionName = SessionName "" }
            , Cmd.batch
                [ get_file_api_not_supported ()
                , Random.generate
                    GotRandomSessionName
                    (Random.map
                        (\int -> String.fromInt int |> Sha256.sha224 |> String.left 16 |> SessionName)
                        (Random.int Random.minInt Random.maxInt)
                    )
                ]
            )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case model of
        LoadingSession loading ->
            case msg of
                GotRandomSessionName sessionName ->
                    ( LoadingSession { loading | sessionName = sessionName }
                    , Cmd.batch
                        [ Lamdera.sendToBackend (LoadSessionRequest sessionName)
                        , Browser.Navigation.replaceUrl loading.key ("/" ++ sessionNameToString sessionName)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        LoadedSession loaded ->
            let
                ( newLoaded, cmd ) =
                    updateLoaded msg loaded
            in
            ( LoadedSession newLoaded, cmd )


sessionNameToString : SessionName -> String
sessionNameToString (SessionName sessionName) =
    sessionName


setEventVisibility : Int -> Bool -> LoadedData -> ( LoadedData, Cmd frontendMsg )
setEventVisibility index isHidden model =
    case Array.get index model.history of
        Just event ->
            if event.isHidden == isHidden then
                ( model, Cmd.none )

            else
                ( { model | history = Array.set index { event | isHidden = isHidden } model.history }
                , SetEventVisibilityRequest { index = index, isHidden = isHidden } |> Lamdera.sendToBackend
                )

        Nothing ->
            ( model, Cmd.none )


updateLoaded : FrontendMsg -> LoadedData -> ( LoadedData, Cmd FrontendMsg )
updateLoaded msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        PressedResetSession ->
            ( model, Lamdera.sendToBackend ResetSessionRequest )

        GotRandomSessionName _ ->
            ( model, Cmd.none )

        ScrolledToBottom ->
            ( model, Cmd.none )

        MouseDownOnEvent index isHidden ->
            setEventVisibility index isHidden { model | mouseDownOnEvent = True }

        MouseEnterOnEvent index isHidden ->
            if model.mouseDownOnEvent then
                setEventVisibility index isHidden model

            else
                ( model, Cmd.none )

        PressedSaveFile ->
            case model.parsedCode of
                ParseSuccess ok ->
                    ( model
                    , codegen ok model (Array.toList model.history |> List.filter (\event -> not event.isHidden))
                        |> write_file_to_js
                    )

                _ ->
                    ( model, Cmd.none )

        ElmUiMsg msg2 ->
            ( { model | elmUiState = Ui.Anim.update ElmUiMsg msg2 model.elmUiState }, Cmd.none )

        ToggledIncludeScreenPos bool ->
            ( { model | includeScreenPos = bool }
            , SetIncludeScreenPageClientPos
                { includeScreenPos = bool
                , includePagePos = model.includePagePos
                , includeClientPos = model.includeClientPos
                }
                |> Lamdera.sendToBackend
            )

        ToggledIncludeClientPos bool ->
            ( { model | includeClientPos = bool }
            , SetIncludeScreenPageClientPos
                { includeScreenPos = model.includeScreenPos
                , includePagePos = model.includePagePos
                , includeClientPos = bool
                }
                |> Lamdera.sendToBackend
            )

        ToggledIncludePagePos bool ->
            ( { model | includePagePos = bool }
            , SetIncludeScreenPageClientPos
                { includeScreenPos = model.includeScreenPos
                , includePagePos = bool
                , includeClientPos = model.includeClientPos
                }
                |> Lamdera.sendToBackend
            )

        MouseUp ->
            ( { model | mouseDownOnEvent = False }, Cmd.none )

        PressedEvent ->
            ( model, Cmd.none )

        GotFile { name, content } ->
            ( { model
                | parsedCode =
                    case parseCode content of
                        Ok ok ->
                            ParseSuccess ok

                        Err err ->
                            ParseFailed err
              }
            , Cmd.none
            )

        PressedSelectFile ->
            ( model, select_file_to_js () )

        PressedNewFile ->
            ( { model | parsedCode = ParseSuccess newCode }
            , Cmd.none
            )

        FileApiNotSupportedFromPort ->
            let
                _ =
                    Debug.log "a" "12356"
            in
            ( { model | parsedCode = FileApiNotSupported }, Cmd.none )


addEvent : Event -> { a | history : Array Event } -> { a | history : Array Event }
addEvent event model =
    let
        event2 : Event
        event2 =
            { event
                | isHidden =
                    case event.eventType of
                        KeyUp keyEvent ->
                            shouldHideKeyEvent keyEvent

                        KeyDown keyEvent ->
                            shouldHideKeyEvent keyEvent

                        _ ->
                            False
            }

        history2 : Array Event
        history2 =
            case Array.get (Array.length model.history - 1) model.history of
                Just last ->
                    if event2.timestamp - last.timestamp < 0 then
                        Array.push event2 model.history
                            |> Array.toList
                            |> List.sortBy .timestamp
                            |> Array.fromList

                    else
                        Array.push event2 model.history

                Nothing ->
                    Array.push event2 model.history
    in
    { model
        | history =
            Array.foldr
                (\item state ->
                    case state.latestEvent of
                        Just { timestamp, previous } ->
                            if timestamp - item.timestamp > 2000 then
                                state

                            else
                                { latestEvent =
                                    Just
                                        { timestamp = timestamp
                                        , previous =
                                            if item.isHidden then
                                                previous

                                            else
                                                item
                                        }
                                , array =
                                    case ( previous.eventType, item.eventType, previous.clientId == item.clientId ) of
                                        ( Input _, Input _, True ) ->
                                            Array.set state.index { item | isHidden = True } state.array

                                        _ ->
                                            state.array
                                , index = state.index - 1
                                }

                        Nothing ->
                            { state | latestEvent = Just { timestamp = item.timestamp, previous = item }, index = state.index - 1 }
                )
                { index = Array.length history2 - 1, latestEvent = Nothing, array = history2 }
                history2
                |> .array
    }


shouldHideKeyEvent : KeyEvent -> Bool
shouldHideKeyEvent keyEvent =
    (String.length keyEvent.key == 1 || keyEvent.key == "Shift" || keyEvent.key == "Backspace")
        && not keyEvent.altKey
        && not keyEvent.ctrlKey
        && not keyEvent.metaKey


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        LoadSessionResponse events ->
            case model of
                LoadingSession loading ->
                    ( LoadedSession
                        { key = loading.key
                        , sessionName = loading.sessionName
                        , history = events.history
                        , copyCounter = 0
                        , elmUiState = Ui.Anim.init
                        , includeScreenPos = events.includeScreenPos
                        , includePagePos = events.includePagePos
                        , includeClientPos = events.includeClientPos
                        , mouseDownOnEvent = False
                        , parsedCode = WaitingOnUser
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SessionUpdate event ->
            case model of
                LoadedSession loaded ->
                    ( addEvent event loaded |> LoadedSession
                    , Browser.Dom.setViewportOf eventsListContainer 0 99999
                        |> Task.attempt (\_ -> ScrolledToBottom)
                    )

                LoadingSession _ ->
                    ( model, Cmd.none )

        ResetSession ->
            case model of
                LoadedSession loaded ->
                    ( LoadedSession { loaded | history = Array.empty }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


parseCodeHelper : String -> Int -> Int -> Int -> Result ParseError ParsedCode
parseCodeHelper code httpRequestsStart portRequestsStart testsStart =
    let
        fromListIndices : List Int
        fromListIndices =
            String.indexes "|> Dict.fromList" code
    in
    case
        ( List.Extra.find (\index -> index > httpRequestsStart) fromListIndices
        , List.Extra.find (\index -> index > portRequestsStart) fromListIndices
        , List.Extra.find (\index -> index > testsStart) (String.indexes "\n    ]" code)
        )
    of
        ( Just httpRequestsEnd, Just portRequestsEnd, Just testsEnd ) ->
            let
                httpRequestsResult =
                    String.slice httpRequestsStart httpRequestsEnd code |> parseHttpRequests

                portRequestsResult =
                    String.slice portRequestsStart portRequestsEnd code |> parsePortRequests

                sorted =
                    List.sortBy
                        (\( a, _, _ ) -> a)
                        [ ( httpRequestsStart, httpRequestsEnd, HttpRequestCode )
                        , ( portRequestsStart, portRequestsEnd, PortRequestCode )
                        , ( testsEnd, testsEnd, TestEntryPoint )
                        ]

                last : Int
                last =
                    case List.reverse sorted of
                        ( _, end, _ ) :: _ ->
                            end

                        [] ->
                            String.length code - 1
            in
            case ( httpRequestsResult, portRequestsResult ) of
                ( Ok httpRequests, Ok portRequests ) ->
                    { codeParts =
                        List.foldl
                            (\( start, end, codeType ) state ->
                                { codeParts =
                                    codeType
                                        :: UserCode (String.slice state.previousIndex start code)
                                        :: state.codeParts
                                , previousIndex = end
                                }
                            )
                            { previousIndex = 0, codeParts = [] }
                            sorted
                            |> .codeParts
                            |> (\a -> UserCode (String.slice last (String.length code - 1) code) :: a)
                            |> List.reverse
                    , httpRequests = httpRequests
                    , portRequests = portRequests
                    , noPriorTests =
                        case String.slice testsStart testsEnd code |> String.split "\n    [" of
                            [ _, rest ] ->
                                String.contains "," rest |> not

                            _ ->
                                False
                    }
                        |> Ok

                ( Err (), Ok _ ) ->
                    Err InvalidHttpRequests

                ( Ok _, Err () ) ->
                    Err InvalidPortRequests

                ( Err (), Err () ) ->
                    Err InvalidHttpAndPortRequests

        ( Nothing, _, _ ) ->
            Err PortRequestsEndNotFound

        ( _, Nothing, _ ) ->
            Err PortRequestsEndNotFound

        ( _, _, Nothing ) ->
            Err TestEntryPointNotFound


parseCode : String -> Result ParseError ParsedCode
parseCode code =
    case ( String.indexes "\nhttpRequests =" code, String.indexes "\nportRequests =" code, String.indexes "\ntests : " code ) of
        ( [ httpRequestsStart ], [ portRequestsStart ], [ testsStart ] ) ->
            parseCodeHelper code httpRequestsStart portRequestsStart testsStart

        ( [], [ _ ], [ _ ] ) ->
            Err PortRequestsNotFound

        ( [ _ ], [], [ _ ] ) ->
            Err PortRequestsNotFound

        ( [ _ ], [ _ ], [] ) ->
            Err TestEntryPointNotFound

        _ ->
            Err UnknownError


newCode : ParsedCode
newCode =
    { codeParts =
        [ UserCode """module MyTests exposing (main, setup, tests)

import Backend
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Effect.Browser.Dom as Dom
import Effect.Lamdera
import Effect.Test as T exposing (FileUpload(..), HttpRequest, HttpResponse(..), MultipleFilesUpload(..), PointerOptions(..))
import Frontend
import Json.Decode
import Json.Encode
import Test.Html.Query
import Test.Html.Selector as Selector
import Time
import Types exposing (ToBackend, FrontendMsg, FrontendModel, ToFrontend, BackendMsg, BackendModel)
import Url exposing (Url)


setup : T.ViewerWith (List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel))
setup =
    T.viewerWith tests
        |> T.addBytesFiles (Dict.values httpRequests)


main : Program () (T.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (T.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    T.startViewer setup


domain : Url
domain =
    { protocol = Url.Http, host = "localhost", port_ = Just 8000, path = "", query = Nothing, fragment = Nothing }


stringToJson : String -> Json.Encode.Value
stringToJson json =
    Result.withDefault Json.Encode.null (Json.Decode.decodeString Json.Decode.value json)


handlePortToJs : { currentRequest : T.PortToJs, data : T.Data FrontendModel BackendModel } -> Maybe ( String, Json.Decode.Value )
handlePortToJs { currentRequest } =
    Dict.get currentRequest.portName portRequests


{-| Please don't modify or rename this function -}
portRequests : Dict String (String, Json.Encode.Value)
portRequests =
    [ """
        , PortRequestCode
        , UserCode """
    ]
        |> Dict.fromList


{-| Please don't modify or rename this function -}
httpRequests : Dict String String
httpRequests =
    [ """
        , HttpRequestCode
        , UserCode """
    ]
        |> Dict.fromList


handleHttpRequests : Dict String Bytes -> { currentRequest : HttpRequest, data : T.Data FrontendModel BackendModel } -> HttpResponse
handleHttpRequests httpData { currentRequest } =
    case Dict.get (currentRequest.method ++ "_" ++ currentRequest.url) httpRequests of
        Just filepath ->
            case Dict.get filepath httpData of
                Just data ->
                    BytesHttpResponse { url = currentRequest.url, statusCode = 200, statusText = "OK", headers = Dict.empty } data

                Nothing ->
                    UnhandledHttpRequest

        Nothing ->
            UnhandledHttpRequest


{-| You can change parts of this function represented with `...`.
The rest needs to remain unchanged in order for the test generator to be able to add new tests.

    tests : ... -> List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
    tests ... =
        let
            config = ...

            ...
        in
        [ ...
        ]
-}
tests : Dict String Bytes -> List (T.Instructions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests httpData =
    let
        config =
            T.Config
                Frontend.app_
                Backend.app_
                (handleHttpRequests httpData)
                handlePortToJs
                (\\_ -> UnhandledFileUpload)
                (\\_ -> UnhandledMultiFileUpload)
                domain
    in
    ["""
        , TestEntryPoint
        , UserCode "\n    ]"
        ]
    , httpRequests = []
    , portRequests = []
    , noPriorTests = True
    }


parseHttpRequests : String -> Result () (List ( String, String ))
parseHttpRequests code =
    case Elm.Parser.parseToFile ("module A exposing (..)\n" ++ code) of
        Ok ast ->
            case ast.declarations of
                [ Node _ (FunctionDeclaration func) ] ->
                    case Node.value func.declaration |> .expression of
                        Node _ (ListExpr requests) ->
                            List.filterMap
                                (\(Node _ request) ->
                                    case request of
                                        TupledExpression [ Node _ (Literal a), Node _ (Literal b) ] ->
                                            Just ( a, b )

                                        _ ->
                                            Nothing
                                )
                                requests
                                |> Ok

                        _ ->
                            Err ()

                _ ->
                    Err ()

        Err _ ->
            Err ()


parsePortRequests : String -> Result () (List ( String, ( String, String ) ))
parsePortRequests code =
    case Elm.Parser.parseToFile ("module A exposing (..)\n" ++ code) of
        Ok ast ->
            case ast.declarations of
                [ Node _ (FunctionDeclaration func) ] ->
                    case Node.value func.declaration |> .expression of
                        Node _ (ListExpr requests) ->
                            List.filterMap
                                (\(Node _ request) ->
                                    case request of
                                        TupledExpression [ Node _ (Literal a), Node _ (TupledExpression [ Node _ (Literal b), Node _ (Application [ _, Node _ (Literal json) ]) ]) ] ->
                                            Just ( a, ( b, json ) )

                                        _ ->
                                            Nothing
                                )
                                requests
                                |> Ok

                        _ ->
                            Err ()

                _ ->
                    Err ()

        Err _ ->
            Err ()


eventsListContainer : String
eventsListContainer =
    "eventsListContainer"


type EventType2
    = Input2 ClientId { targetId : String, text : String }
    | Click2 ClientId { targetId : String }
    | ClickLink2 ClientId LinkEvent
    | Connect2 ClientId ConnectEvent
    | KeyUp2 ClientId KeyEvent
    | KeyDown2 ClientId KeyEvent
    | PointerDown2 ClientId PointerEvent
    | PointerUp2 ClientId PointerEvent
    | PointerMove2 ClientId PointerEvent
    | PointerLeave2 ClientId PointerEvent
    | PointerCancel2 ClientId PointerEvent
    | PointerOver2 ClientId PointerEvent
    | PointerEnter2 ClientId PointerEvent
    | PointerOut2 ClientId PointerEvent
    | TouchStart2 ClientId TouchEvent
    | TouchCancel2 ClientId TouchEvent
    | TouchMove2 ClientId TouchEvent
    | TouchEnd2 ClientId TouchEvent
    | FromJsPort2 ClientId { port_ : String, data : String }
    | WindowResize2 ClientId WindowResizeEvent
    | SimulateTime Int
    | CheckView2 ClientId CheckViewEvent


eventsToEvent2 : List Event -> List EventType2
eventsToEvent2 events =
    List.foldl
        (\{ clientId, eventType, timestamp } state ->
            case eventType of
                Input input ->
                    { previousEvent = Just { eventType = Input2 clientId input, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                KeyDown keyDown ->
                    { previousEvent = Just { eventType = KeyDown2 clientId keyDown, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                KeyUp keyUp ->
                    { previousEvent = Just { eventType = KeyUp2 clientId keyUp, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                PointerDown a ->
                    { previousEvent = Just { eventType = PointerDown2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                PointerUp a ->
                    { previousEvent = Just { eventType = PointerUp2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                PointerMove a ->
                    { previousEvent = Just { eventType = PointerMove2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                PointerLeave a ->
                    { previousEvent = Just { eventType = PointerLeave2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                PointerCancel a ->
                    { previousEvent = Just { eventType = PointerCancel2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                PointerOver a ->
                    { previousEvent = Just { eventType = PointerOver2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                PointerEnter a ->
                    { previousEvent = Just { eventType = PointerEnter2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                PointerOut a ->
                    { previousEvent = Just { eventType = PointerOut2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                TouchStart a ->
                    { previousEvent = Just { eventType = TouchStart2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                TouchCancel a ->
                    { previousEvent = Just { eventType = TouchCancel2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                TouchMove a ->
                    { previousEvent = Just { eventType = TouchMove2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                TouchEnd a ->
                    { previousEvent = Just { eventType = TouchEnd2 clientId a, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                Connect connect ->
                    { previousEvent = Just { eventType = Connect2 clientId connect, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                Click mouseEvent ->
                    case mouseEvent.targetId of
                        Just targetId ->
                            { previousEvent = Just { eventType = Click2 clientId { targetId = targetId }, time = timestamp }
                            , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                            }

                        Nothing ->
                            state

                ClickLink linkEvent ->
                    { previousEvent = Just { eventType = ClickLink2 clientId linkEvent, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                Http _ ->
                    state

                HttpLocal _ ->
                    state

                Paste pasteEvent ->
                    case pasteEvent.targetId of
                        Just targetId ->
                            { previousEvent = Just { eventType = Input2 clientId { targetId = targetId, text = pasteEvent.text }, time = timestamp }
                            , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                            }

                        Nothing ->
                            state

                ResetBackend ->
                    state

                FromJsPort fromJsPort ->
                    case fromJsPort.triggeredFromPort of
                        Just _ ->
                            state

                        Nothing ->
                            { previousEvent =
                                { eventType = FromJsPort2 clientId { port_ = fromJsPort.port_, data = fromJsPort.data }
                                , time = timestamp
                                }
                                    |> Just
                            , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                            }

                WindowResize resizeEvent ->
                    { previousEvent = Just { eventType = WindowResize2 clientId resizeEvent, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }

                CheckView checkView ->
                    { previousEvent = Just { eventType = CheckView2 clientId checkView, time = timestamp }
                    , rest = Maybe.Extra.toList state.previousEvent ++ state.rest
                    }
        )
        { previousEvent = Nothing, rest = [] }
        events
        |> (\state -> Maybe.Extra.toList state.previousEvent ++ state.rest)
        |> List.reverse
        |> List.foldl
            (\event state ->
                { previousEvent = Just event
                , list =
                    case state.previousEvent of
                        Just { time } ->
                            let
                                delta : Int
                                delta =
                                    event.time - time
                            in
                            if delta > 0 then
                                event.eventType :: SimulateTime delta :: state.list

                            else
                                event.eventType :: state.list

                        Nothing ->
                            event.eventType :: state.list
                }
            )
            { previousEvent = Nothing, list = [] }
        |> .list
        |> List.reverse


dropPrefix : String -> String -> String
dropPrefix prefix text =
    if String.startsWith prefix text then
        String.dropLeft (String.length prefix) text

    else
        text


codegen : ParsedCode -> LoadedData -> List Event -> String
codegen parsedCode model events =
    let
        tests =
            List.Extra.groupWhile (\a _ -> a.eventType /= ResetBackend) events
                |> List.filter (\( _, rest ) -> not (List.isEmpty rest))

        testsText : String
        testsText =
            tests
                |> List.indexedMap (\index ( head, rest ) -> testCode model head.timestamp index (head :: rest))
                |> String.join "\n    ,"
                |> (\a ->
                        if parsedCode.noPriorTests || List.isEmpty tests then
                            a

                        else
                            "\n    ," ++ a
                   )

        httpRequests : String
        httpRequests =
            List.filterMap
                (\event ->
                    case event.eventType of
                        Http http ->
                            ( http.method ++ "_" ++ dropPrefix "http://localhost:8001/" http.url, http.filepath ) |> Just

                        _ ->
                            Nothing
                )
                events
                |> (\a -> parsedCode.httpRequests ++ a ++ localRequests)
                |> Dict.fromList
                |> Dict.toList
                |> List.map (\( first, second ) -> "(\"" ++ first ++ "\", \"" ++ second ++ "\")")
                |> String.join "\n    , "
                |> (\a -> "\nhttpRequests =\n    [ " ++ a ++ "\n    ]\n        ")

        localRequests : List ( String, String )
        localRequests =
            List.filterMap
                (\event ->
                    case event.eventType of
                        HttpLocal { filepath } ->
                            Just ( "GET_" ++ filepath, "/public" ++ filepath )

                        _ ->
                            Nothing
                )
                events

        portRequests : String
        portRequests =
            List.filterMap
                (\event ->
                    case event.eventType of
                        FromJsPort fromJsPort ->
                            case fromJsPort.triggeredFromPort of
                                Just trigger ->
                                    Just { triggeredFromPort = trigger, port_ = fromJsPort.port_, data = fromJsPort.data }

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing
                )
                events
                |> (\a -> List.map (\( b, ( c, d ) ) -> { triggeredFromPort = b, port_ = c, data = d }) parsedCode.portRequests ++ a)
                |> List.Extra.uniqueBy (\a -> a.triggeredFromPort)
                |> List.map
                    (\fromJsPort ->
                        "(\""
                            ++ fromJsPort.triggeredFromPort
                            ++ "\", (\""
                            ++ fromJsPort.port_
                            ++ "\", stringToJson "
                            ++ (if String.contains "\"" fromJsPort.data then
                                    "\"\"\"" ++ fromJsPort.data ++ "\"\"\""

                                else
                                    "\"" ++ fromJsPort.data ++ "\""
                               )
                            ++ "))"
                    )
                |> String.join "\n    , "
                |> (\a -> "\nportRequests =\n    [ " ++ a ++ "\n    ]\n        ")
    in
    List.map
        (\codePart ->
            case codePart of
                UserCode code ->
                    code

                HttpRequestCode ->
                    httpRequests

                PortRequestCode ->
                    portRequests

                TestEntryPoint ->
                    testsText
        )
        parsedCode.codeParts
        |> String.concat


testCode : { a | includeClientPos : Bool, includePagePos : Bool, includeScreenPos : Bool } -> Int -> Int -> List Event -> String
testCode includes startTime testIndex events =
    let
        clients : List ClientId
        clients =
            List.map .clientId events |> AssocSet.fromList |> AssocSet.toList
    in
    List.foldl
        (\event { code, indentation, clientCount } ->
            let
                indent =
                    String.repeat indentation "             "

                client clientId =
                    case List.Extra.findIndex (\a -> a == clientId) clients of
                        Just index ->
                            "tab" ++ String.fromInt (index + 1)

                        Nothing ->
                            "tab"
            in
            case event of
                Connect2 clientId { url, sessionId, windowWidth, windowHeight } ->
                    let
                        state =
                            if clientCount == 0 then
                                "state"

                            else
                                "state" ++ String.fromInt clientCount
                    in
                    { code =
                        code
                            ++ (indent ++ "    |> T.connectFrontend\n")
                            ++ (indent ++ "        (Effect.Lamdera.sessionIdFromString \"" ++ sessionId ++ "\")\n")
                            ++ (indent ++ "        (Url.fromString \"" ++ url ++ "\" |> Maybe.withDefault domain)\n")
                            ++ (indent ++ "        { width = " ++ String.fromInt windowWidth ++ ", height = " ++ String.fromInt windowHeight ++ " }\n")
                            ++ (indent ++ "        (\\( " ++ state ++ ", " ++ client clientId ++ ") ->\n")
                            ++ (indent ++ "            " ++ state ++ "\n")
                    , indentation = indentation + 1
                    , clientCount = clientCount + 1
                    }

                WindowResize2 clientId resizeEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".resizeWindow { width = "
                            ++ String.fromInt resizeEvent.width
                            ++ ", height = "
                            ++ String.fromInt resizeEvent.height
                            ++ " }\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                KeyDown2 clientId keyEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".keyDown "
                            ++ targetIdFunc keyEvent.targetId
                            ++ " \""
                            ++ keyEvent.key
                            ++ "\" [ "
                            ++ String.join ", "
                                (List.filterMap
                                    (\( name, bool ) ->
                                        if bool then
                                            Just name

                                        else
                                            Nothing
                                    )
                                    [ ( "Key_ShiftHeld", keyEvent.shiftKey )
                                    , ( "Key_AltHeld", keyEvent.altKey )
                                    , ( "Key_CtrlHeld", keyEvent.ctrlKey )
                                    , ( "Key_MetaHeld", keyEvent.metaKey )
                                    ]
                                )
                            ++ " ]\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                KeyUp2 clientId keyEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".keyUp "
                            ++ targetIdFunc keyEvent.targetId
                            ++ " \""
                            ++ keyEvent.key
                            ++ "\" [ "
                            ++ String.join ", "
                                (List.filterMap
                                    (\( name, bool ) ->
                                        if bool then
                                            Just name

                                        else
                                            Nothing
                                    )
                                    [ ( "Key_ShiftHeld", keyEvent.shiftKey )
                                    , ( "Key_AltHeld", keyEvent.altKey )
                                    , ( "Key_CtrlHeld", keyEvent.ctrlKey )
                                    , ( "Key_MetaHeld", keyEvent.metaKey )
                                    ]
                                )
                            ++ " ]\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                Click2 clientId mouseEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".clickButton "
                            ++ targetIdFunc mouseEvent.targetId
                            ++ "\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                ClickLink2 clientId mouseEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".clickLink \""
                            ++ mouseEvent.path
                            ++ "\"\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                Input2 clientId { targetId, text } ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".inputText "
                            ++ targetIdFunc targetId
                            ++ " \""
                            ++ text
                            ++ "\"\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                FromJsPort2 _ _ ->
                    { code = code
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerDown2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerDown" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerUp2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerUp" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerMove2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerMove" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerLeave2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerLeave" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerCancel2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerCancel" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerOver2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerOver" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerEnter2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerEnter" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                PointerOut2 clientId a ->
                    { code = code ++ indent ++ pointerCodegen includes "pointerOut" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchStart2 clientId a ->
                    { code = code ++ indent ++ touchCodegen "touchStart" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchCancel2 clientId a ->
                    { code = code ++ indent ++ touchCodegen "touchCancel" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchMove2 clientId a ->
                    { code = code ++ indent ++ touchCodegen "touchMove" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                TouchEnd2 clientId a ->
                    { code = code ++ indent ++ touchCodegen "touchEnd" (client clientId) a
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                SimulateTime duration ->
                    { code = code ++ indent ++ "    |> T.wait (Duration.milliseconds " ++ String.fromInt duration ++ ")\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }

                CheckView2 clientId checkViewEvent ->
                    { code =
                        code
                            ++ indent
                            ++ "    |> "
                            ++ client clientId
                            ++ ".checkView (Test.Html.Query.has [ "
                            ++ String.join ", " (List.map (\text -> "Selector.text \"" ++ text ++ "\"") checkViewEvent.selection)
                            ++ " ])\n"
                    , indentation = indentation
                    , clientCount = clientCount
                    }
        )
        { code =
            " T.start \"test"
                ++ String.fromInt testIndex
                ++ "\" (Time.millisToPosix "
                ++ String.fromInt startTime
                ++ ") config"
                ++ "\n"
        , indentation = 0
        , clientCount = 0
        }
        (eventsToEvent2 events)
        |> (\{ code, indentation } -> code ++ "        " ++ String.repeat indentation ")")


touchCodegen : String -> String -> TouchEvent -> String
touchCodegen funcName client a =
    let
        touchToString : Touch -> String
        touchToString touch =
            "{ id = "
                ++ String.fromInt touch.identifier
                ++ ", screenPos = ("
                ++ String.fromFloat touch.screenX
                ++ ", "
                ++ String.fromFloat touch.screenY
                ++ "), clientPos = ("
                ++ String.fromFloat touch.clientX
                ++ ", "
                ++ String.fromFloat touch.clientY
                ++ "), pagePos = ("
                ++ String.fromFloat touch.pageX
                ++ ", "
                ++ String.fromFloat touch.pageY
                ++ ") }"
    in
    "    |> "
        ++ client
        ++ "."
        ++ funcName
        ++ " "
        ++ targetIdFunc a.targetId
        ++ " { targetTouches = [ "
        ++ String.join ", " (List.map touchToString a.targetTouches)
        ++ " ], changedTouches = [ "
        ++ String.join ", " (List.map touchToString a.targetTouches)
        ++ " ] }\n"


pointerCodegen : { a | includeClientPos : Bool, includePagePos : Bool, includeScreenPos : Bool } -> String -> String -> PointerEvent -> String
pointerCodegen { includeClientPos, includePagePos, includeScreenPos } funcName client a =
    let
        options : List String
        options =
            List.filterMap
                (\( name, include, ( x, y ) ) ->
                    if (x == a.offsetX && y == a.offsetY) || not include then
                        Nothing

                    else
                        Just (name ++ " " ++ String.fromFloat x ++ " " ++ String.fromFloat y)
                )
                [ ( "ScreenXY", includeScreenPos, ( a.screenX, a.screenY ) )
                , ( "PageXY", includePagePos, ( a.pageX, a.pageY ) )
                , ( "ClientXY", includeClientPos, ( a.clientX, a.clientY ) )
                ]
                ++ List.filterMap
                    identity
                    [ case a.button of
                        1 ->
                            Just "PointerButton MainButton"

                        2 ->
                            Just "PointerButton MiddleButton"

                        3 ->
                            Just "PointerButton SecondButton"

                        4 ->
                            Just "PointerButton BackButton"

                        5 ->
                            Just "PointerButton ForwardButton"

                        _ ->
                            Nothing
                    , if a.altKey then
                        Just "AltHeld"

                      else
                        Nothing
                    , if a.shiftKey then
                        Just "ShiftHeld"

                      else
                        Nothing
                    , if a.ctrlKey then
                        Just "CtrlHeld"

                      else
                        Nothing
                    , if a.metaKey then
                        Just "MetaHeld"

                      else
                        Nothing
                    , if a.pointerId == 0 then
                        Nothing

                      else
                        Just ("PointerId " ++ String.fromInt a.pointerId)
                    , if a.isPrimary then
                        Nothing

                      else
                        Just "IsNotPrimary"
                    ]
    in
    "    |> "
        ++ client
        ++ "."
        ++ funcName
        ++ " "
        ++ targetIdFunc a.targetId
        ++ " ("
        ++ String.fromFloat a.offsetX
        ++ ","
        ++ String.fromFloat a.offsetY
        ++ ") [ "
        ++ String.join ", " options
        ++ " ]\n"


targetIdFunc : String -> String
targetIdFunc id =
    "(Dom.id \"" ++ id ++ "\")"


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ case model of
            LoadingSession _ ->
                Html.text "Loading session..."

            LoadedSession loaded ->
                Ui.Anim.layout
                    { options = []
                    , breakpoints = Nothing
                    , toMsg = ElmUiMsg
                    }
                    loaded.elmUiState
                    [ Ui.Font.family [ Ui.Font.sansSerif ], Ui.height Ui.fill ]
                    (loadedView loaded)
        ]
    }


loadedView : LoadedData -> Ui.Element FrontendMsg
loadedView model =
    case model.parsedCode of
        WaitingOnUser ->
            Ui.column
                [ Ui.centerX, Ui.centerY, Ui.spacing 16, Ui.width Ui.shrink ]
                [ Ui.el
                    [ Ui.Input.button PressedNewFile
                    , Ui.border 1
                    , Ui.borderColor (Ui.rgb 100 100 100)
                    , Ui.background (Ui.rgb 240 240 240)
                    , Ui.padding 8
                    , Ui.Font.size 20
                    , Ui.rounded 8
                    ]
                    (Ui.text "New end-to-end test module")
                , Ui.el
                    [ Ui.Input.button PressedSelectFile
                    , Ui.border 1
                    , Ui.borderColor (Ui.rgb 100 100 100)
                    , Ui.background (Ui.rgb 240 240 240)
                    , Ui.padding 8
                    , Ui.Font.size 20
                    , Ui.rounded 8
                    ]
                    (Ui.text "Open end-to-end test module")
                ]

        ParseSuccess parsed ->
            let
                eventsList : List Event
                eventsList =
                    Array.toList model.history
            in
            Ui.row
                [ Ui.height Ui.fill ]
                [ Ui.column
                    [ Ui.height Ui.fill
                    , Ui.width Ui.shrink
                    , Ui.background (Ui.rgb 255 250 245)
                    , Ui.borderWith { left = 0, right = 1, top = 0, bottom = 0 }
                    , Ui.borderColor (Ui.rgb 245 240 235)
                    ]
                    [ Ui.row
                        []
                        [ Ui.el [ Ui.Font.bold, Ui.paddingWith { left = 8, right = 8, top = 8, bottom = 4 } ] (Ui.text "Unsaved events")
                        , Ui.el
                            [ Ui.Input.button PressedResetSession
                            , Ui.border 1
                            , Ui.borderColor (Ui.rgb 120 110 100)
                            , Ui.background (Ui.rgb 240 235 230)
                            , Ui.width Ui.shrink
                            , Ui.paddingWith { left = 8, right = 8, top = 10, bottom = 6 }
                            ]
                            (Ui.text "Clear")
                        , Ui.el
                            [ Ui.Input.button PressedSaveFile
                            , Ui.border 1
                            , Ui.borderColor (Ui.rgb 120 110 100)
                            , Ui.background (Ui.rgb 240 235 230)
                            , Ui.width Ui.shrink
                            , Ui.paddingWith { left = 8, right = 8, top = 10, bottom = 6 }
                            ]
                            (Ui.text "Save to file")
                        ]
                    , eventsView eventsList
                    ]
                , Ui.column
                    [ Ui.height Ui.fill, Ui.spacing 0 ]
                    ([ Ui.column
                        [ Ui.spacing 8, Ui.padding 8 ]
                        [ Ui.Prose.paragraph
                            [ Ui.Font.size 16, Ui.width Ui.shrink, Ui.spacing 4 ]
                            [ Ui.text "Press "
                            , Ui.el
                                [ Ui.Font.color (Ui.rgb 238 238 238)
                                , Ui.background (Ui.rgb 41 51 53)
                                , Ui.Font.size 14
                                , Ui.contentCenterY
                                , Ui.padding 4
                                ]
                                (Ui.text "Reset\u{00A0}Backend")
                            , Ui.text " in lamdera live to end the current test and start a new test"
                            ]
                        , Ui.column
                            []
                            [ simpleCheckbox ToggledIncludeClientPos "Include clientPos in pointer events" model.includeClientPos
                            , simpleCheckbox ToggledIncludePagePos "Include pagePos in pointer events" model.includePagePos
                            , simpleCheckbox ToggledIncludeScreenPos "Include screenPos in pointer events" model.includeScreenPos
                            ]
                        ]
                     , codegen parsed model (List.filter (\event -> not event.isHidden) eventsList)
                        |> Ui.text
                        |> Ui.el
                            [ Ui.Font.family [ Ui.Font.monospace ]
                            , Ui.Font.size 12
                            , Ui.Font.exactWhitespace
                            , Ui.scrollable
                            , Ui.padding 8
                            ]
                     ]
                     --(case model.parsedCode of
                     --    ParseSuccess _ ->
                     --        [ Ui.el
                     --            [ Ui.borderWith { left = 0, top = 1, bottom = 0, right = 0 }
                     --            , Ui.borderColor (Ui.rgb 100 100 100)
                     --            , Ui.inFront
                     --                (Ui.row
                     --                    [ Ui.Input.button PressedCopyCode
                     --                    , Ui.border 1
                     --                    , Ui.borderColor (Ui.rgb 100 100 100)
                     --                    , Ui.background (Ui.rgb 240 240 240)
                     --                    , Ui.width Ui.shrink
                     --                    , Ui.padding 4
                     --                    , Ui.roundedWith { topLeft = 0, topRight = 0, bottomRight = 0, bottomLeft = 4 }
                     --                    , Ui.alignRight
                     --                    , Ui.move { x = 0, y = -1, z = 0 }
                     --                    , Ui.Font.size 14
                     --                    , Ui.spacing 4
                     --                    ]
                     --                    [ Icons.copy
                     --                    , if model.copyCounter > 0 then
                     --                        Ui.text "Copied!"
                     --
                     --                      else
                     --                        Ui.text "Copy to clipboard"
                     --                    ]
                     --                )
                     --            ]
                     --            Ui.none
                     --        ]
                     --
                     --    _ ->
                     --        []
                     --)
                    )
                ]

        ParseFailed error ->
            Ui.el
                []
                ((case error of
                    InvalidPortRequests ->
                        "The portRequests function was found but couldn't be parsed."

                    InvalidHttpRequests ->
                        "The httpRequests function was found but couldn't be parsed."

                    InvalidHttpAndPortRequests ->
                        "The portRequests and httpRequests functions were found but couldn't be parsed."

                    PortRequestsNotFound ->
                        "The portRequests function wasn't found."

                    HttpRequestsNotFound ->
                        "The httpRequests function wasn't found."

                    TestEntryPointNotFound ->
                        "The test entry point (the \"]\" at the end of the the tests function) wasn't found."

                    UnknownError ->
                        "This end-to-end test module appears to be corrupted or the wrong file is being loaded."

                    PortRequestsEndNotFound ->
                        "The portRequests function was found but it's supposed to end with \"|> Dict.fromList\""

                    HttpRequestsEndNotFound ->
                        "The httpRequests function was found but it's supposed to end with \"|> Dict.fromList\""
                 )
                    |> Ui.text
                )

        FileApiNotSupported ->
            Ui.el
                [ Ui.centerX, Ui.centerY, Ui.widthMax 400, Ui.Font.size 20 ]
                (Ui.text "Your browser doesn't support the File System API. It's needed in order to read and write to your end-to-end test module. It should work on Chrome (sorry).")


simpleCheckbox : (Bool -> msg) -> String -> Bool -> Ui.Element msg
simpleCheckbox msg text value =
    let
        { element, id } =
            Ui.Input.label text [ Ui.Font.size 14, Ui.move { x = 0, y = 2, z = 0 } ] (Ui.text text)
    in
    Ui.row
        [ Ui.spacing 4 ]
        [ Ui.Input.checkbox
            []
            { onChange = msg
            , icon = Nothing
            , checked = value
            , label = id
            }
        , element
        ]


eventsView : List Event -> Ui.Element FrontendMsg
eventsView events =
    case events of
        [] ->
            Ui.el [ Ui.width (Ui.px 240), Ui.padding 16 ] (Ui.text "No events have arrived")

        _ ->
            List.indexedMap
                (\index event ->
                    let
                        text : String
                        text =
                            case event.eventType of
                                KeyDown keyEvent ->
                                    keyEvent.key ++ " key down"

                                KeyUp keyEvent ->
                                    keyEvent.key ++ " key up"

                                Click mouseEvent ->
                                    case mouseEvent.targetId of
                                        Just id ->
                                            "Click " ++ id

                                        Nothing ->
                                            "Click nothing"

                                Http _ ->
                                    "Http request"

                                Connect record ->
                                    "Connected " ++ record.sessionId

                                ClickLink linkEvent ->
                                    "Click link " ++ linkEvent.path

                                Paste pasteEvent ->
                                    "Pasted text " ++ ellipsis pasteEvent.text

                                Input inputEvent ->
                                    "Text input " ++ ellipsis inputEvent.text

                                ResetBackend ->
                                    "Test ended"

                                FromJsPort fromJsPortEvent ->
                                    "Port " ++ fromJsPortEvent.port_ ++ " " ++ ellipsis fromJsPortEvent.data

                                HttpLocal { filepath } ->
                                    "Loaded " ++ filepath

                                WindowResize { width, height } ->
                                    "Window resized w:" ++ String.fromInt width ++ " h:" ++ String.fromInt height

                                PointerDown _ ->
                                    "Pointer down"

                                PointerUp _ ->
                                    "Pointer up"

                                PointerMove _ ->
                                    "Pointer move"

                                PointerLeave _ ->
                                    "Pointer leave"

                                PointerCancel _ ->
                                    "Pointer cancel"

                                PointerOver _ ->
                                    "Pointer over"

                                PointerEnter _ ->
                                    "Pointer enter"

                                PointerOut _ ->
                                    "Pointer out"

                                TouchStart _ ->
                                    "Touch start"

                                TouchCancel _ ->
                                    "Touch cancel"

                                TouchMove _ ->
                                    "Touch move"

                                TouchEnd _ ->
                                    "Touch end"

                                CheckView _ ->
                                    "Check view"
                    in
                    Ui.row
                        [ Ui.Input.button PressedEvent
                        , Ui.Events.onMouseDown (MouseDownOnEvent index (not event.isHidden))
                        , Ui.Events.onMouseEnter (MouseEnterOnEvent index (not event.isHidden))
                        , Ui.spacing 4
                        , Ui.Font.color
                            (if event.isHidden then
                                Ui.rgb 120 120 120

                             else
                                Ui.rgb 0 0 0
                            )
                        , Ui.htmlAttribute (Html.Attributes.title text)
                        ]
                        [ if event.isHidden then
                            Icons.eyeClosed

                          else
                            Icons.eye
                        , Ui.text text
                        ]
                )
                events
                |> Ui.column
                    [ Ui.width (Ui.px 240)
                    , Ui.Font.size 14
                    , Ui.paddingXY 8 0
                    , Ui.clipWithEllipsis
                    , Ui.paddingWith { left = 0, right = 0, top = 4, bottom = 8 }
                    ]
                |> Ui.el [ Ui.id eventsListContainer, Ui.scrollable, Ui.height Ui.fill, Ui.width Ui.shrink ]


ellipsis : String -> String
ellipsis text =
    if String.length text < 10 then
        text

    else
        String.left 7 text ++ "..."
