module Internal.RadioButton.Model exposing
    ( defaultModel
    , Model
    , Msg(..)
    )

import Internal.Ripple.Model as Ripple


type alias Model =
    { ripple : Ripple.Model
    , isFocused : Bool
    }


defaultModel : Model
defaultModel =
    { ripple = Ripple.defaultModel
    , isFocused = False
    }


type Msg
    = RippleMsg Ripple.Msg
    | NoOp
    | SetFocus Bool
