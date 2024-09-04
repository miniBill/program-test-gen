module Evergreen.V1.Types exposing (..)

import Array
import AssocList
import AssocSet
import Browser
import Browser.Navigation
import Evergreen.V1.SessionName
import Evergreen.V1.Ui.Anim
import Lamdera
import Url


type alias LoadingData =
    { key : Browser.Navigation.Key
    , sessionName : Evergreen.V1.SessionName.SessionName
    }


type alias KeyEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , key : String
    }


type alias ClickEvent =
    { targetId : Maybe String
    }


type alias LinkEvent =
    { path : String
    }


type alias HttpEvent =
    { responseType : String
    , method : String
    , url : String
    , filepath : String
    }


type alias HttpLocalEvent =
    { filepath : String
    }


type alias ConnectEvent =
    { url : String
    , sessionId : Lamdera.SessionId
    , windowWidth : Int
    , windowHeight : Int
    }


type alias PasteEvent =
    { targetId : Maybe String
    , text : String
    }


type alias InputEvent =
    { targetId : String
    , text : String
    }


type alias FromJsPortEvent =
    { triggeredFromPort : Maybe String
    , port_ : String
    , data : String
    }


type alias WindowResizeEvent =
    { width : Int
    , height : Int
    }


type alias PointerEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , clientX : Float
    , clientY : Float
    , offsetX : Float
    , offsetY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , button : Int
    , pointerType : String
    , pointerId : Int
    , isPrimary : Bool
    , width : Float
    , height : Float
    , pressure : Float
    , tiltX : Float
    , tiltY : Float
    }


type alias Touch =
    { clientX : Float
    , clientY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , identifier : Int
    }


type alias TouchEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , changedTouches : List Touch
    , targetTouches : List Touch
    , touches : List Touch
    }


type alias CheckViewEvent =
    { selection : List String
    }


type alias MouseEvent =
    { targetId : String
    , ctrlKey : Bool
    , shiftKey : Bool
    , metaKey : Bool
    , altKey : Bool
    , clientX : Float
    , clientY : Float
    , offsetX : Float
    , offsetY : Float
    , pageX : Float
    , pageY : Float
    , screenX : Float
    , screenY : Float
    , button : Int
    }


type alias FocusEvent =
    { targetId : String
    }


type alias BlurEvent =
    { targetId : String
    }


type EventType
    = KeyDown KeyEvent
    | KeyUp KeyEvent
    | Click ClickEvent
    | ClickLink LinkEvent
    | Http HttpEvent
    | HttpLocal HttpLocalEvent
    | Connect ConnectEvent
    | Paste PasteEvent
    | Input InputEvent
    | ResetBackend
    | FromJsPort FromJsPortEvent
    | WindowResize WindowResizeEvent
    | PointerDown PointerEvent
    | PointerUp PointerEvent
    | PointerMove PointerEvent
    | PointerLeave PointerEvent
    | PointerCancel PointerEvent
    | PointerOver PointerEvent
    | PointerEnter PointerEvent
    | PointerOut PointerEvent
    | TouchStart TouchEvent
    | TouchCancel TouchEvent
    | TouchMove TouchEvent
    | TouchEnd TouchEvent
    | CheckView CheckViewEvent
    | MouseDown MouseEvent
    | MouseUp MouseEvent
    | MouseMove MouseEvent
    | MouseLeave MouseEvent
    | MouseOver MouseEvent
    | MouseEnter MouseEvent
    | MouseOut MouseEvent
    | Focus FocusEvent
    | Blur BlurEvent


type alias Event =
    { isHidden : Bool
    , timestamp : Int
    , eventType : EventType
    , clientId : Lamdera.ClientId
    }


type alias Settings =
    { includeClientPos : Bool
    , includePagePos : Bool
    , includeScreenPos : Bool
    , showAllCode : Bool
    }


type Code
    = UserCode String
    | HttpRequestCode
    | PortRequestCode
    | TestEntryPoint


type alias ParsedCode =
    { codeParts : List Code
    , httpRequests : List ( String, String )
    , portRequests : List ( String, ( String, String ) )
    , noPriorTests : Bool
    }


type ParseError
    = InvalidPortRequests
    | InvalidHttpRequests
    | InvalidHttpAndPortRequests
    | PortRequestsNotFound
    | HttpRequestsNotFound
    | TestEntryPointNotFound
    | PortRequestsEndNotFound
    | HttpRequestsEndNotFound
    | UnknownError


type ParsedCodeStatus
    = ParseSuccess ParsedCode
    | ParseFailed ParseError
    | WaitingOnUser
    | FileApiNotSupported


type CommitStatus
    = NotCommitted
    | Committing String
    | CommitSuccess
    | CommitFailed


type alias LoadedData =
    { key : Browser.Navigation.Key
    , sessionName : Evergreen.V1.SessionName.SessionName
    , history : Array.Array Event
    , copyCounter : Int
    , elmUiState : Evergreen.V1.Ui.Anim.State
    , settings : Settings
    , parsedCode : ParsedCodeStatus
    , mouseDownOnEvent : Bool
    , commitStatus : CommitStatus
    , noEventsHaveArrived : Bool
    }


type FrontendModel
    = LoadingSession LoadingData
    | LoadedSession LoadedData


type alias Session =
    { history : Array.Array Event
    , connections : AssocSet.Set Lamdera.ClientId
    , settings : Settings
    }


type alias BackendModel =
    { sessions : AssocList.Dict Evergreen.V1.SessionName.SessionName Session
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PressedResetSession
    | GotRandomSessionName Evergreen.V1.SessionName.SessionName
    | ScrolledToBottom
    | MouseDownOnEvent Int Bool
    | MouseEnterOnEvent Int Bool
    | ElmUiMsg Evergreen.V1.Ui.Anim.Msg
    | ToggledIncludeScreenPos Bool
    | ToggledIncludeClientPos Bool
    | ToggledIncludePagePos Bool
    | MouseUpEvent
    | PressedEvent
    | GotFile
        { name : String
        , content : String
        }
    | PressedSelectFile
    | PressedNewFile
    | PressedCommitToFile
    | FileApiNotSupportedFromPort
    | ToggledShowAllCode Bool
    | WroteToFile Bool
    | PressedDownloadTestGen


type ToBackend
    = LoadSessionRequest Evergreen.V1.SessionName.SessionName
    | ResetSessionRequest
    | SetEventVisibilityRequest
        { index : Int
        , isHidden : Bool
        }
    | SetSettingsRequest Settings


type BackendMsg
    = ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = LoadSessionResponse
        { history : Array.Array Event
        , settings : Settings
        }
    | SessionUpdate Event
    | ResetSession
