module Destroid.Main where

import Time exposing (..)
import Signal exposing (..)

import Destroid.Utils exposing (..)
import Destroid.Model exposing (..)
import Destroid.World exposing (..)
import Destroid.Debug exposing (..)
import Destroid.Views exposing (..)
import Destroid.Control exposing (..)


deltas : Signal Float
deltas = Signal.map (\t -> t/20) (fps 40)

input : Signal (Float, World)
input = Signal.sampleOn deltas (Signal.map2 (,) deltas world)

states : Signal Model
states = foldp updater istate input


main = map2 view world states


{-             arrows   space
                  \     /
                   \   /
     fps 40     commands
       |             |    dimensions
       |             |   /
      deltas        world
          \        /   |
           \      /    |
            input      |
              |        |
            states    /
                \    /
                 \  /
                 main                 -}
