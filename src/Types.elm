module Types exposing (..)

import Array exposing (Array)
import AssocList
import AssocSet
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
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
    , history : Array Event
    }


type alias BackendModel =
    { sessions : AssocList.Dict SessionName Session
    }


type alias Session =
    { history : Array Event
    , connections : AssocSet.Set ClientId
    }


type SessionName
    = SessionName String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | PressedResetSession
    | GotRandomSessionName SessionName
    | ScrolledToBottom


type ToBackend
    = LoadSessionRequest SessionName
    | ResetSessionRequest


type BackendMsg
    = ClientDisconnected SessionId ClientId


type ToFrontend
    = LoadSessionResponse (Array Event)
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
    | ClickLink LinkEvent
    | Http HttpEvent
    | Connect { url : String, sessionId : SessionId, windowWidth : Int, windowHeight : Int }


type alias KeyEvent =
    { targetId : Maybe String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , key : String
    , keyCode : Int
    }


type alias MouseEvent =
    { targetId : Maybe String
    }


type alias LinkEvent =
    { path : String
    }


type alias HttpEvent =
    { responseType : String
    , method : String
    , url : String
    , responseHash : String
    }
