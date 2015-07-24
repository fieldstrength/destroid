module Destroid.Model where

import Debug exposing (watch)

import Destroid.Params exposing (spaceSize, stageScale)
import Destroid.Utils  exposing (v2scale)


type Mode = Title
          | Transition Float
          | LevelIntro Float
          | Playing    Float
          | Dead       Float
          | Cleared    Float


-- data for physical bodies
type alias Phys a = { a | x  : Float,  -- x-position
                          y  : Float,  -- y-position
                          r  : Float,  -- angle
                          vx : Float,  -- x-velocity
                          vy : Float,  -- y-velocity
                          vr : Float}  -- angular speed


-- asteroid size
type ASize = Big | Medium | Small

type alias Asteroid = Phys {sz : ASize}

type alias Bullet = Phys {t0 : Float}


type alias Level = { ls : List (Float,Float,ASize),  -- (angle, speed, ASize)
                     ts : List Float,                -- times for asteroid emmissions
                     tf : Float,                     -- time when the intro sequence ends
                     xi : Phys {} }                  -- wormhole location


-- master game data record
type alias Model = { mode   : Mode,           -- game mode
                     screenScale : Float,     -- multiplication factor for display
                     time   : Float,          -- total time
                     dt     : Float,          -- timestep
                     
                     me     : Phys {},        -- the ship
                     buls   : List Bullet,    -- bullets
                     ast    : List Asteroid,  -- list of asteroids
                     lvl    : Level,          -- level data
                     levnum : Int,
                     score  : Int,
                     hi     : Int,
                     
                     f      : Bool,           -- firing
                     trigg  : Bool,           -- gun ready
                     life   : Float,          -- life (out of 100)
                     blink  : Maybe Float}    -- post-hit invulnerability expiration time


-- initial state
istate : Model
istate = { mode        = Title,
           screenScale = 9,
           time        = 0,
           dt          = 0,

           me          = x0,
           buls        = [],
           ast         = [],
           lvl         = l0,
           levnum      = 1,
           score       = 0,
           hi          = 0,

           f           = False,
           trigg       = False,
           life        = 100,
           blink       = Nothing}

x0 = { x  = 0,
       y  = 0,
       r  = 0,
       vx = 0,
       vy = 0,
       vr = 0 }

l0 = { ls = [], ts = [], xi = x0, tf = 0 }


screenCoords : Model -> (Float,Float) -> (Float,Float)
screenCoords m = v2scale m.screenScale

stage : Model -> (Float,Float)
stage m = screenCoords m spaceSize

