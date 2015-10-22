module Main where

import BigPrelude

import Control.Monad.Aff (runAff, forkAff)

import Halogen
import Halogen.Util (appendToBody)
import Control.Monad.Eff.Exception (throwException)

import qualified Router as R

main :: forall eff. Eff (R.Effects eff) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI R.ui (installedState R.init)
  appendToBody app.node
  forkAff $ R.routeSignal app.driver
