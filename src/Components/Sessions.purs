module Component.Sessions where

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Prelude (class Eq, class Ord, type (~>), Unit, Void, const, pure, unit)

data Input a
  = Noop a

type State = Unit

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall m. H.Component HH.HTML Input Unit Void m
ui = H.component
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render _ =
      HH.div_
        [ HH.h1_ [ HH.text "Your Sessions" ]
        , HH.p_ [ HH.text "wow you lift a LOT" ]
        ]

    eval :: Input ~> H.ComponentDSL State Input Void m
    eval (Noop n) = pure n
