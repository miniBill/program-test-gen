module Types exposing (..)

import Array exposing (Array)
import AssocList
import AssocSet
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type FrontendModel
    = LoadingSession LoadingData
    | LoadedSession LoadedData


type alias LoadingData =
    { key : Key, sessionName : SessionName }


type alias LoadedData =
    { key : Key
    , sessionName : SessionName
    , history : List Event
    }


type alias BackendModel =
    { sessions : AssocList.Dict SessionName Session
    }


type alias Session =
    { history : List Event
    , connections : AssocSet.Set ClientId
    }


type SessionName
    = SessionName String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedResetSession
    | GotRandomSessionName SessionName


type ToBackend
    = LoadSessionRequest SessionName
    | ResetSessionRequest


type BackendMsg
    = ClientDisconnected SessionId ClientId


type ToFrontend
    = LoadSessionResponse (List Event)
    | SessionUpdate Event
    | ResetSession


type alias Event =
    { timestamp : Int
    , eventType : EventType
    , clientId : ClientId
    }


type EventType
    = KeyDown KeyEvent
    | KeyUp KeyEvent
    | Click MouseEvent
    | Http HttpEvent


type alias KeyEvent =
    { currentTargetId : String
    , targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , key : String
    , keyCode : Int
    }


type alias MouseEvent =
    { currentTargetId : String
    , targetId : String
    }


type alias HttpEvent =
    { responseType : String
    , method : String
    , url : String
    }
