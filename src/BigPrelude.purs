module BigPrelude
  ( module Prelude
  , module Data.Maybe
  , module Data.Either
  , module Effect
  , module Effect.Class
  , module Data.Functor
  , module Control.Alt
  , module Data.Tuple
  , module Control.Apply
  , module Control.Plus
  , eitherToMaybe
  , eitherToList
  , eitherToArray
  ) where

import Prelude
import Control.Plus
import Control.Alt
import Control.Apply
import Data.Functor
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Array as A
import Data.List as L
import Data.List (List())
import Effect
import Effect.Class

eitherToMaybe :: forall a b. Either b a -> Maybe a
eitherToMaybe (Left _) =
  Nothing
eitherToMaybe (Right a) =
  Just a

eitherToList :: forall a b. Either b a -> List a
eitherToList (Left _) =
  L.Nil
eitherToList (Right a) =
  L.Cons a L.Nil

eitherToArray :: forall a b. Either b a -> Array a
eitherToArray (Left _) =
  []
eitherToArray (Right a) =
  [a]
