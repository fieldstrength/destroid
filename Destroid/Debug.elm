module Destroid.Debug where

import Graphics.Element exposing (..)
import Text exposing (fromString)


show_fps : number -> Element
show_fps n = flow right [leftAligned <| fromString "Frames per second:  ", show (50/n)]

show_step : number -> Element
show_step n = flow right [leftAligned (fromString "Time step [ms]:  "), 
                          show (n*20),
                          leftAligned (fromString "        dt:  "),
                          show n]

debug_info : number -> Element
debug_info n = flow down [show_fps n, show_step n]

{- Note: The fps function gives time deltas in miliseconds. 
We divide these deltas by 20 to get a dt of around unity. 
Hence the conversions above are needed. -}
