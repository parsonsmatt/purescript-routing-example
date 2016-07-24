module Component.Profile where

import Prelude

import Halogen
import Halogen.HTML.Indexed as H

data Input a
  = Noop a

type State = Unit

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall g. (Functor g) => Component State Input g
ui = component { render, eval }
  where
    render _ =
      H.div_
        [ H.h1_ [ H.text "Your Profile" ]
        , H.p_ [ H.text "what a nice profile!" ]
        ]

    eval :: Input ~> ComponentDSL State Input g
    eval (Noop n) = pure n
