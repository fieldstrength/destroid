module Destroid.Debug where

import Graphics.Element exposing (..)
import Text exposing (fromString)
import Graphics.Collage exposing (collage, toForm, Form, move)
import Color exposing (orange)


show_fps : number -> Element
show_fps n = flow right [leftAligned <| fromString "Frames per second:  ", show (50/n)]

show_step : number -> Element
show_step n = flow right [leftAligned (fromString "Time step [ms]:  "), 
                          show (n*20),
                          leftAligned (fromString "        dt:  "),
                          show n]

debug_info : number -> Element
debug_info n = flow down [show_fps n, show_step n]

debug_panel : (Int,Int) -> number -> Form
debug_panel (cw,ch) dt =
  debug_info dt
    |> color orange
    |> width 300
    |> toForm
    |> move (160 - 0.5 * toFloat cw,30 - 0.5 * toFloat ch)

{- Note: The fps function gives time deltas in miliseconds. 
We divide these deltas by 20 to get a dt of around unity. 
Hence the conversions above are needed. -}

