module Router where

import BigPrelude

import Data.Functor.Coproduct (Coproduct(..))
import Control.Monad.Aff (Aff(), forkAff)
import qualified Control.Monad.Aff as AF
import Control.Monad.Eff.Exception
import Control.Monad.Aff.AVar
import DOM
import Control.Monad.Free (liftFI)

import Data.String (toLower)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P
import Halogen.Component.ChildPath (ChildPath(), cpR, cpL)

import Routing
import Routing.Match
import Routing.Match.Class

import qualified Component.Profile as Profile
import qualified Component.Sessions as Sessions

data Input a 
  = Goto Routes a

data CRUD
  = Index
  | Show Number

data Routes
  = Profile
  | Sessions CRUD
  | Home

init :: State
init = { currentPage: "Home" }

routing :: Match Routes
routing = profile
      <|> sessions
      <|> home
  where
    profile = Profile <$ route "profile"
    home = Home <$ lit ""
    sessions = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> num <|> pure Index

type State =
  { currentPage :: String 
  }

type ChildState = Either Profile.State Sessions.State
type ChildQuery = Coproduct Profile.Input Sessions.Input
type ChildSlot = Either Profile.Slot Sessions.Slot

pathToProfile :: ChildPath Profile.State ChildState Profile.Input ChildQuery Profile.Slot ChildSlot
pathToProfile = cpL

pathToSessions :: ChildPath Sessions.State ChildState Sessions.Input ChildQuery Sessions.Slot ChildSlot
pathToSessions = cpR

type StateP g
  = InstalledState State ChildState Input ChildQuery g ChildSlot

type QueryP
  = Coproduct Input (ChildF ChildSlot ChildQuery)

ui :: forall g. (Plus g) 
   => Component (StateP g) QueryP g
ui = parentComponent render eval
  where
    render st =
      H.div_
        [ H.h1_ [ H.text (st.currentPage) ]
        , H.ul_ (map link ["Sessions", "Profile", "Home"])
        , viewPage st.currentPage
        ]

    link s = H.li_ [ H.a [ P.href ("#/" ++ toLower s) ] [ H.text s ] ]

    viewPage :: String -> HTML (SlotConstructor ChildState ChildQuery g ChildSlot) Input
    viewPage "Sessions" =
      H.slot' pathToSessions Sessions.Slot \_ -> { component: Sessions.ui, initialState: unit }
    viewPage "Profile" =
      H.slot' pathToProfile Profile.Slot \_ -> { component: Profile.ui, initialState: unit }
    viewPage _ =
      H.div_ []

    eval :: EvalParent Input State ChildState Input ChildQuery g ChildSlot
    eval (Goto Profile next) = do
      modify (_ { currentPage = "Profile" })
      pure next
    eval (Goto (Sessions view) next) = do
      modify case view of
                  Index -> (_ { currentPage = "Sessions" })
                  Show n -> (_ { currentPage = "Session " ++ show n })
      pure next
    eval (Goto Home next) = do
      modify (_ { currentPage = "Home" })
      pure next

type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)

routeSignal :: forall eff. Driver QueryP eff
            -> Aff (Effects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. Driver QueryP eff
          -> Maybe Routes
          -> Routes
          -> Aff (Effects eff) Unit
redirects driver _ =
  driver <<< Coproduct <<< Left <<< action <<< Goto
-- redirects driver _ Home = 
--   driver (Coproduct (Left (action (Goto Home))))
-- redirects driver _ Profile =
--   driver (Coproduct (Left (action (Goto Profile))))
-- redirects driver _ (Sessions view) =
--   driver (Coproduct (Left (action (Goto (Sessions view)))))
