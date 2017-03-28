module Main where

import BigPrelude

import Control.Monad.Aff (forkAff)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Router as R

main :: forall eff. Eff (HA.HalogenEffects eff) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI R.ui unit body
  forkAff $ R.routeSignal driver
