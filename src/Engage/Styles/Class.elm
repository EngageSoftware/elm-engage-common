module Engage.Styles.Class exposing
    ( Class(..)
    , Importance(..)
    , MessageType(..)
    , Size(..)
    , Visibility(..)
    )


type Class
    = FormControl Size


type Size
    = Small
    | Large


type Importance
    = Standard
    | Primary
    | Divert
    | Negative


type MessageType
    = Confirmation
    | Error
    | Warning
    | Info


type Visibility
    = Visible
    | Hidden
