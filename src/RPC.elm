module RPC exposing (..)

import Array
import AssocList
import AssocSet
import Codec exposing (Codec)
import Frontend
import Json.Decode
import Json.Encode
import Lamdera
import LamderaRPC exposing (HttpBody(..))
import Set
import Types exposing (BackendModel, BackendMsg(..), Event, SessionName(..), ToFrontend(..))


type alias EventEndpoint =
    { event : Event
    , sessionName : SessionName
    }


eventEndpointCodec : Codec EventEndpoint
eventEndpointCodec =
    Codec.object EventEndpoint
        |> Codec.field "event" .event eventCodec
        |> Codec.field "sessionName" .sessionName sessionNameCodec
        |> Codec.buildObject


sessionNameCodec : Codec SessionName
sessionNameCodec =
    Codec.map SessionName (\(SessionName a) -> a) Codec.string


eventCodec : Codec Event
eventCodec =
    Codec.object (Event False)
        |> Codec.field "timestamp" .timestamp Codec.int
        |> Codec.field "eventType" .eventType eventTypeCodec
        |> Codec.field "clientId" .clientId Codec.string
        |> Codec.buildObject


eventTypeCodec : Codec Types.EventType
eventTypeCodec =
    Codec.custom
        (\a b c d e f g h i j k l value ->
            case value of
                Types.KeyDown arg0 ->
                    a arg0

                Types.KeyUp arg0 ->
                    b arg0

                Types.Click arg0 ->
                    c arg0

                Types.ClickLink arg0 ->
                    d arg0

                Types.Http arg0 ->
                    e arg0

                Types.Connect arg0 ->
                    f arg0

                Types.Paste arg0 ->
                    g arg0

                Types.Input arg0 ->
                    h arg0

                Types.ResetBackend ->
                    i

                Types.FromJsPort arg0 ->
                    j arg0

                Types.HttpLocal arg0 ->
                    k arg0

                Types.WindowResize arg0 ->
                    l arg0
        )
        |> Codec.variant1 "KeyDown" Types.KeyDown keyEventCodec
        |> Codec.variant1 "KeyUp" Types.KeyUp keyEventCodec
        |> Codec.variant1 "Click" Types.Click mouseEventCodec
        |> Codec.variant1 "ClickLink" Types.ClickLink linkEventCodec
        |> Codec.variant1 "Http" Types.Http httpEventCodec
        |> Codec.variant1 "Connect" Types.Connect connectEventCodec
        |> Codec.variant1 "Paste" Types.Paste pasteEventCodec
        |> Codec.variant1 "Input" Types.Input inputEventCodec
        |> Codec.variant0 "ResetBackend" Types.ResetBackend
        |> Codec.variant1 "FromJsPort" Types.FromJsPort fromJsPortEventCodec
        |> Codec.variant1 "HttpLocal" Types.HttpLocal httpLocalEventCodec
        |> Codec.variant1 "WindowResize" Types.WindowResize windowResizeEventCodec
        |> Codec.buildCustom


connectEventCodec : Codec Types.ConnectEvent
connectEventCodec =
    Codec.object Types.ConnectEvent
        |> Codec.field "url" .url Codec.string
        |> Codec.field "sessionId" .sessionId Codec.string
        |> Codec.field "windowWidth" .windowWidth Codec.int
        |> Codec.field "windowHeight" .windowHeight Codec.int
        |> Codec.nullableField "code" .code Codec.string
        |> Codec.buildObject


keyEventCodec : Codec Types.KeyEvent
keyEventCodec =
    Codec.object Types.KeyEvent
        |> Codec.field "targetId" .targetId Codec.string
        |> Codec.field "ctrlKey" .ctrlKey Codec.bool
        |> Codec.field "shiftKey" .shiftKey Codec.bool
        |> Codec.field "metaKey" .metaKey Codec.bool
        |> Codec.field "altKey" .metaKey Codec.bool
        |> Codec.field "key" .key Codec.string
        |> Codec.buildObject


mouseEventCodec : Codec Types.MouseEvent
mouseEventCodec =
    Codec.object Types.MouseEvent
        |> Codec.field "targetId" .targetId (Codec.nullable Codec.string)
        |> Codec.buildObject


pasteEventCodec : Codec Types.PasteEvent
pasteEventCodec =
    Codec.object Types.PasteEvent
        |> Codec.field "targetId" .targetId (Codec.nullable Codec.string)
        |> Codec.field "text" .text Codec.string
        |> Codec.buildObject


inputEventCodec : Codec Types.InputEvent
inputEventCodec =
    Codec.object Types.InputEvent
        |> Codec.field "targetId" .targetId Codec.string
        |> Codec.field "text" .text Codec.string
        |> Codec.buildObject


fromJsPortEventCodec : Codec Types.FromJsPortEvent
fromJsPortEventCodec =
    Codec.object Types.FromJsPortEvent
        |> Codec.nullableField "triggeredFromPort" .triggeredFromPort Codec.string
        |> Codec.field "port" .port_ Codec.string
        |> Codec.field "data" .data Codec.string
        |> Codec.buildObject


httpLocalEventCodec : Codec Types.HttpLocalEvent
httpLocalEventCodec =
    Codec.object Types.HttpLocalEvent
        |> Codec.field "filepath" .filepath Codec.string
        |> Codec.buildObject


windowResizeEventCodec : Codec Types.WindowResizeEvent
windowResizeEventCodec =
    Codec.object Types.WindowResizeEvent
        |> Codec.field "width" .width Codec.int
        |> Codec.field "height" .height Codec.int
        |> Codec.buildObject


linkEventCodec : Codec Types.LinkEvent
linkEventCodec =
    Codec.object Types.LinkEvent
        |> Codec.field "path" .path Codec.string
        |> Codec.buildObject


httpEventCodec : Codec Types.HttpEvent
httpEventCodec =
    Codec.object Types.HttpEvent
        |> Codec.field "responseType" .responseType Codec.string
        |> Codec.field "method" .method Codec.string
        |> Codec.field "url" .url Codec.string
        |> Codec.field "filepath" .filepath Codec.string
        |> Codec.buildObject


lamdera_handleEndpoints :
    Json.Encode.Value
    -> LamderaRPC.HttpRequest
    -> BackendModel
    -> ( LamderaRPC.RPCResult, BackendModel, Cmd BackendMsg )
lamdera_handleEndpoints _ args model =
    case args.endpoint of
        "event" ->
            case args.body of
                BodyString json ->
                    case Codec.decodeString eventEndpointCodec json |> Debug.log "result" of
                        Ok { sessionName, event } ->
                            let
                                model2 : BackendModel
                                model2 =
                                    { model
                                        | sessions =
                                            AssocList.update
                                                sessionName
                                                (\maybeSession ->
                                                    (case maybeSession of
                                                        Just session ->
                                                            Frontend.addEvent event session

                                                        Nothing ->
                                                            { history = Array.fromList [ event ]
                                                            , connections = AssocSet.empty
                                                            }
                                                    )
                                                        |> Just
                                                )
                                                model.sessions
                                    }
                            in
                            ( LamderaRPC.ResultString ""
                            , model2
                            , broadcastToClients sessionName (SessionUpdate event) model2
                            )

                        Err error ->
                            ( Json.Decode.errorToString error |> LamderaRPC.ResultString
                            , model
                            , Cmd.none
                            )

                _ ->
                    ( LamderaRPC.ResultString ""
                    , model
                    , Cmd.none
                    )

        _ ->
            ( LamderaRPC.ResultString ""
            , model
            , Cmd.none
            )


broadcastToClients : SessionName -> ToFrontend -> BackendModel -> Cmd BackendMsg
broadcastToClients sessionName toFrontend model =
    case AssocList.get sessionName model.sessions of
        Just session ->
            AssocSet.toList session.connections
                |> List.map (\clientId -> Lamdera.sendToFrontend clientId toFrontend)
                |> Cmd.batch

        Nothing ->
            Cmd.none
