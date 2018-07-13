module Main where

import BigPrelude

import Effect (Effect)
import Effect.Aff (forkAff)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Router as R

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI R.ui unit body
  forkAff $ R.routeSignal driver
