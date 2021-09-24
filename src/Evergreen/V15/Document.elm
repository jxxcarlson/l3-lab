module Evergreen.V15.Document exposing (..)

import Evergreen.V15.Common.Syntax
import Time


type alias Username =
    String


type Access
    = Public
    | Private
    | Shared
        { canRead : List Username
        , canWrite : List Username
        }


type alias Document =
    { title : String
    , author : String
    , username : String
    , id : String
    , created : Time.Posix
    , modified : Time.Posix
    , tags : List String
    , content : String
    , access : Access
    , slug : Maybe String
    , language : Evergreen.V15.Common.Syntax.Language
    }
