module Destroid.Main where

import Time             exposing (fps)
import Signal           exposing (map2, foldp)

import Destroid.Model   exposing (Model, istate)
import Destroid.World   exposing (World, world)
import Destroid.Views   exposing (view)
import Destroid.Control exposing (updater)


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
