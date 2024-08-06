module RPC exposing (..)

import AssocList
import AssocSet
import Codec exposing (Codec)
import Json.Decode
import Json.Encode
import Lamdera
import LamderaRPC exposing (HttpBody(..))
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
    Codec.custom
        (\sessionNameEncoder value ->
            case value of
                Types.SessionName arg0 ->
                    sessionNameEncoder arg0
        )
        |> Codec.variant1 "SessionName" SessionName Codec.string
        |> Codec.buildCustom


eventCodec : Codec Event
eventCodec =
    Codec.object Event
        |> Codec.field "timestamp" .timestamp Codec.int
        |> Codec.field "eventType" .eventType eventTypeCodec
        |> Codec.field "clientId" .clientId Codec.string
        |> Codec.buildObject


eventTypeCodec : Codec Types.EventType
eventTypeCodec =
    Codec.custom
        (\keyDownEncoder keyUpEncoder clickEncoder httpEncoder connectEncoder value ->
            case value of
                Types.KeyDown arg0 ->
                    keyDownEncoder arg0

                Types.KeyUp arg0 ->
                    keyUpEncoder arg0

                Types.Click arg0 ->
                    clickEncoder arg0

                Types.Http arg0 ->
                    httpEncoder arg0

                Types.Connect arg0 ->
                    connectEncoder arg0
        )
        |> Codec.variant1 "KeyDown" Types.KeyDown keyEventCodec
        |> Codec.variant1 "KeyUp" Types.KeyUp keyEventCodec
        |> Codec.variant1 "Click" Types.Click mouseEventCodec
        |> Codec.variant1 "Http" Types.Http httpEventCodec
        |> Codec.variant1
            "Connect"
            Types.Connect
            (Codec.object
                (\url sessionId windowWidth windowHeight ->
                    { url = url, sessionId = sessionId, windowWidth = windowWidth, windowHeight = windowHeight }
                )
                |> Codec.field "url" .url Codec.string
                |> Codec.field "sessionId" .sessionId Codec.string
                |> Codec.field "windowWidth" .windowWidth Codec.int
                |> Codec.field "windowHeight" .windowHeight Codec.int
                |> Codec.buildObject
            )
        |> Codec.buildCustom


keyEventCodec : Codec Types.KeyEvent
keyEventCodec =
    Codec.object Types.KeyEvent
        |> Codec.field "currentTargetId" .currentTargetId (Codec.nullable Codec.string)
        |> Codec.field "targetId" .targetId (Codec.nullable Codec.string)
        |> Codec.field "ctrlKey" .ctrlKey Codec.bool
        |> Codec.field "shiftKey" .shiftKey Codec.bool
        |> Codec.field "metaKey" .metaKey Codec.bool
        |> Codec.field "key" .key Codec.string
        |> Codec.field "keyCode" .keyCode Codec.int
        |> Codec.buildObject


mouseEventCodec : Codec Types.MouseEvent
mouseEventCodec =
    Codec.object Types.MouseEvent
        |> Codec.field "currentTargetId" .currentTargetId (Codec.nullable Codec.string)
        |> Codec.field "targetId" .targetId (Codec.nullable Codec.string)
        |> Codec.buildObject


httpEventCodec : Codec Types.HttpEvent
httpEventCodec =
    Codec.object Types.HttpEvent
        |> Codec.field "responseType" .responseType Codec.string
        |> Codec.field "method" .method Codec.string
        |> Codec.field "url" .url Codec.string
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
                BodyJson json ->
                    case Codec.decodeValue eventEndpointCodec json of
                        Ok { sessionName, event } ->
                            ( LamderaRPC.ResultString ""
                            , model
                            , broadcastToClients sessionName (SessionUpdate event) model
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
