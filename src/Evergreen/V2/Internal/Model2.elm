module Evergreen.V2.Internal.Model2 exposing (..)

import Evergreen.V2.Internal.Teleport
import Set
import Time


type State
    = State
        { added : Set.Set String
        , rules : List String
        , keyframes : List String
        }


type Msg
    = Tick Time.Posix
    | Teleported Evergreen.V2.Internal.Teleport.Trigger Evergreen.V2.Internal.Teleport.Event
