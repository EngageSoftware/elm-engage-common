module Engage.Styles.Class exposing
    ( Class(..)
    , Importance(..)
    , MessageType(..)
    , Size(..)
    , Visibility(..)
    )

{-| Styles.Class

@docs Class, Importance, MessageType, Size, Visibility

-}


{-| The Class type
-}
type Class
    = FormControl Size


{-| The Size type
-}
type Size
    = Small
    | Large


{-| The Importance type
-}
type Importance
    = Standard
    | Primary
    | Divert
    | Negative


{-| The MessageType type
-}
type MessageType
    = Confirmation
    | Error
    | Warning
    | Info


{-| The Visibility type
-}
type Visibility
    = Visible
    | Hidden
