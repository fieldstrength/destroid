module Destroid.Main where

import Time             exposing (fpsWhen)
import Signal           exposing (map2, foldp)
import Keyboard         exposing (shift)

import Destroid.Model   exposing (Model, istate)
import Destroid.World   exposing (World, world, running)
import Destroid.Views   exposing (view)
import Destroid.Control exposing (updater)


deltas : Signal Float
deltas = Signal.map (\t -> t/20) (fpsWhen 40 running)

input : Signal (Float, World)
input = Signal.sampleOn deltas (Signal.map2 (,) deltas world)

states : Signal Model
states = foldp updater istate input


main = map2 view world states


{-
 isDown (ESC)   arrows   space
        |           \     /
        |            \   /
     fpsWhen      commands
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
