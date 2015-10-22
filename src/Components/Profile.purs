module Component.Profile where

import Prelude
import Data.Generic

import Halogen
import qualified Halogen.HTML.Indexed as H

data Input a
  = Noop a

type State = Unit

data Slot = Slot

derive instance slotGeneric :: Generic Slot

instance eqSlot :: Eq Slot where
  eq = gEq

instance ordGeneric :: Ord Slot where
  compare = gCompare

ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render _ =
      H.div_
        [ H.h1_ [ H.text "Your Profile" ]
        , H.p_ [ H.text "what a nice profile!" ]
        ]

    eval :: Eval _ _ _ g
    eval (Noop n) = pure n
