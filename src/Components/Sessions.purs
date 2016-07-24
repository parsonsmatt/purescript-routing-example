module Component.Sessions where

import Prelude
import Data.Generic

import Halogen
import Halogen.HTML.Indexed as H

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
ui = component { render, eval }
  where
    render _ =
      H.div_
        [ H.h1_ [ H.text "Your Sessions" ]
        , H.p_ [ H.text "wow you lift a LOT" ]
        ]

    eval :: Input ~> ComponentDSL State Input g
    eval (Noop n) = pure n
