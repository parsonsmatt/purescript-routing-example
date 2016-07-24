module Main where

import BigPrelude

import Control.Monad.Aff (runAff, forkAff)

import Halogen
import Halogen.Util
import Control.Monad.Eff.Exception (throwException)

import Router as R

main :: forall eff. Eff (R.Effects eff) Unit
main = void $ runAff throwException (const (pure unit)) $ do
  body <- awaitBody
  driver <- runUI R.ui (parentState R.init) body
  forkAff $ R.routeSignal driver
