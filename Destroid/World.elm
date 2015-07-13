module Destroid.World where

import Keyboard exposing (space)
import Window


type alias Arrows = { x : Int, y : Int }
 
type Steer = SLeft | SCenter | SRight

type alias Command = { steer  : Steer,
                       thrust : Bool,
                       shoot  : Bool }

type alias World = { c : Command,
                     w : Int,   -- window width
                     h : Int }  -- window height

mkSteer : Arrows -> Steer
mkSteer arr = case (arr.x) of
  -1 -> SLeft
  0  -> SCenter
  1  -> SRight

mkCommand : Arrows -> Bool -> Command
mkCommand arr b = case (arr.y) of
  1 -> { steer = (mkSteer arr), thrust = True,  shoot = b }
  _ -> { steer = (mkSteer arr), thrust = False, shoot = b }


commands : Signal Command
commands = Signal.map2 mkCommand Keyboard.arrows space


mkWorld cc (ww,hh) = { c = cc, w = ww, h = hh }


world : Signal World
world = Signal.map2 mkWorld commands Window.dimensions
