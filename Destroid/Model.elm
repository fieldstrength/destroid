module Destroid.Model where

import Debug exposing (watch)

type Mode = Title
          | Transition Float
          | Playing
          | Dead


-- data for physical bodies
type alias Phys = { x  : Float,  -- x-position
                    y  : Float,  -- y-position
                    r  : Float,  -- angle
                    vx : Float,  -- x-velocity
                    vy : Float,  -- y-velocity
                    vr : Float}  -- angular speed

x0 = { x  = 0,
       y  = 0,
       r  = 0,
       vx = 0,
       vy = 0,
       vr = 0 }

-- asteroid size
type ASize = Big | Medium | Small

-- master game data record
type alias Model = { mode   : Mode,           -- game mode
                     aspect : Float,          -- aspect ratio of space. doesn't change
                     size   : (Float, Float), -- size of the compact space.
                     time   : Float,          -- total time
                     dt     : Float,          -- timestep
                     
                     me     : Phys,           -- the ship
                     buls   : List Phys,      -- bullets
                     ast    : List Phys,      -- list of asteroids
                     
                     f      : Bool,           -- firing
                     trigg  : Bool,           -- gun ready
                     life   : Float,          -- life (out of 100)

                     debug  : Bool}           -- debug mode

-- initial state
istate : Model
istate = { mode   = Title,
           aspect = 1.5,
           size   = (900,600),
           time   = 0,
           dt     = 0,

           me     = x0,
           buls   = [],
           ast    = [],

           f      = False,
           trigg  = False,
           life   = 100,

           debug  = False}
