module Tests exposing (expected, test)

import Ansi.Color
import Diff
import Expect
import Frontend
import List.Extra
import Test exposing (Test)
import Types exposing (EventType(..))


test : Test
test =
    Test.describe "Codegen"
        [ Test.test "Default codegen with no tests" <|
            \_ ->
                Frontend.codegen
                    Frontend.newCode
                    { includeClientPos = False
                    , includePagePos = False
                    , includeScreenPos = False
                    , showAllCode = True
                    }
                    []
                    |> expectEqualMultiline (expected "")
        , Test.test "Default codegen with one test" <|
            \_ ->
                Frontend.codegen
                    Frontend.newCode
                    { includeClientPos = False
                    , includePagePos = False
                    , includeScreenPos = False
                    , showAllCode = True
                    }
                    [ { isHidden = False
                      , timestamp = 0
                      , eventType = ResetBackend
                      , clientId = "clientId0"
                      }
                    , { isHidden = False
                      , timestamp = 0
                      , eventType =
                            Connect
                                { url = "https://my-site.com"
                                , sessionId = "sessionId0"
                                , windowWidth = 1000
                                , windowHeight = 800
                                }
                      , clientId = "clientId0"
                      }
                    , { isHidden = False
                      , timestamp = 100
                      , eventType = Click { targetId = Just "start" }
                      , clientId = "clientId0"
                      }
                    , { isHidden = False
                      , timestamp = 200
                      , eventType = Click { targetId = Just "edit" }
                      , clientId = "clientId0"
                      }
                    , { isHidden = False
                      , timestamp = 300
                      , eventType =
                            Connect
                                { url = "https://my-site.com/about"
                                , sessionId = "sessionId0"
                                , windowWidth = 1100
                                , windowHeight = 700
                                }
                      , clientId = "clientId1"
                      }
                    , { isHidden = False
                      , timestamp = 400
                      , eventType = ClickLink { path = "/" }
                      , clientId = "clientId1"
                      }
                    , { isHidden = False
                      , timestamp = 400
                      , eventType =
                            KeyDown
                                { targetId = "loginField"
                                , ctrlKey = False
                                , shiftKey = False
                                , metaKey = False
                                , altKey = False
                                , key = "h"
                                }
                      , clientId = "clientId0"
                      }
                    ]
                    |> expectEqualMultiline
                        (expected
                            """ T.start
        "test0"
        (Time.millisToPosix 0)
        config
        [ T.connectFrontend
            0
            (Effect.Lamdera.sessionIdFromString "sessionId0")
            /my-site.com
            { width = 1000, height = 800 }
            (\\tab1 ->
                [ tab1.click 100 (Dom.id "start")
                , tab1.click 100 (Dom.id "edit")
                , T.connectFrontend
                    100
                    (Effect.Lamdera.sessionIdFromString "sessionId0")
                    /my-site.com/about
                    { width = 1100, height = 700 }
                    (\\tab2 ->
                        [ tab2.clickLink 100 "/"
                        , tab1.keyDown 0 (Dom.id "loginField") "h" [  ]
                        ]
                    )
                ]
            )
        ]"""
                        )
        ]


expectEqualMultiline : String -> String -> Expect.Expectation
expectEqualMultiline exp actual =
    if exp == actual then
        Expect.pass

    else
        let
            context : number
            context =
                4

            diff : List String
            diff =
                Diff.diffWith (\l r -> String.trim l == String.trim r)
                    (String.lines exp)
                    (String.lines actual)
                    |> gatherGroups
                    |> List.concatMap
                        (\( head, tail ) ->
                            case head of
                                Diff.NoChange _ _ ->
                                    if
                                        (List.length tail > 2 * context)
                                            && List.all (\t -> not (isChange t)) tail
                                    then
                                        head
                                            :: List.take (context - 1) tail
                                            ++ Diff.NoChange "" ""
                                            :: Diff.NoChange "---" "---"
                                            :: Diff.NoChange "" ""
                                            :: List.reverse (List.take context (List.reverse tail))

                                    else
                                        head :: tail

                                _ ->
                                    head :: tail
                        )
                    |> List.map changeToString

            header : String
            header =
                Ansi.Color.fontColor Ansi.Color.blue "Diff from expected to actual:"
        in
        Expect.fail (String.join "\n" (header :: diff))


changeToString : Diff.Change String -> String
changeToString change =
    case change of
        Diff.NoChange before after ->
            if before == after then
                " " ++ before

            else
                lineChangeToString before after

        Diff.Added line ->
            Ansi.Color.fontColor Ansi.Color.green ("+" ++ line)

        Diff.Removed line ->
            Ansi.Color.fontColor Ansi.Color.red ("-" ++ line)


lineChangeToString : String -> String -> String
lineChangeToString before after =
    let
        ( befores, afters ) =
            Diff.diff (String.toList before) (String.toList after)
                |> List.foldr
                    (\change ( bacc, aacc ) ->
                        case change of
                            Diff.NoChange c _ ->
                                let
                                    s : String
                                    s =
                                        String.fromChar c
                                in
                                ( s :: bacc
                                , s :: aacc
                                )

                            Diff.Added a ->
                                let
                                    s : String
                                    s =
                                        String.fromChar a
                                            |> Ansi.Color.backgroundColor Ansi.Color.green
                                in
                                ( bacc
                                , s :: aacc
                                )

                            Diff.Removed r ->
                                let
                                    s : String
                                    s =
                                        String.fromChar r
                                            |> Ansi.Color.backgroundColor Ansi.Color.red
                                in
                                ( s :: bacc
                                , aacc
                                )
                    )
                    ( [], [] )
    in
    Ansi.Color.fontColor Ansi.Color.red ("-" ++ String.concat befores)
        ++ "\n"
        ++ Ansi.Color.fontColor Ansi.Color.green ("+" ++ String.concat afters)


gatherGroups : List (Diff.Change a) -> List ( Diff.Change a, List (Diff.Change a) )
gatherGroups list =
    List.Extra.groupWhile (\l r -> isChange l == isChange r) list


isChange : Diff.Change a -> Bool
isChange c =
    case c of
        Diff.NoChange b a ->
            b /= a

        _ ->
            True


expected : String -> String
expected tests =
    """module MyTests exposing (main, setup, tests)

import Backend
import Bytes exposing (Bytes)
import Dict exposing (Dict)
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
    [ 
    ]
        |> Dict.fromList


{-| Please don't modify or rename this function -}
httpRequests : Dict String String
httpRequests =
    [ 
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
        ++ tests
        ++ """
    
    ]"""
        |> String.replace "\u{000D}" ""
