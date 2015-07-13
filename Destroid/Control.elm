module Destroid.Control where

import Debug exposing (watch)

import Destroid.World exposing (..)
import Destroid.Model exposing (..)
import Destroid.Utils exposing (..)


updater : (Float, World) -> Model -> Model
updater w m = m |> timeStep w |> watcher |> case m.mode of
  Playing      -> gameUpdate w
  Title        -> titleUpdate w
  Transition t -> transitionUpdate w (t + 120)
  Dead         -> deadUpdate w


watcher : Model -> Model
watcher m = {m | size <- Debug.watch "Size" m.size,
                 time <- Debug.watch "Time [sec/50]" m.time,
                 me   <- Debug.watch "Flight data" m.me}


timeStep : (Float,World) -> Model -> Model
timeStep (dt,w) m = {m | time <- m.time + dt,
                         dt   <- dt}

---------------------------------------
--            Title screen
---------------------------------------

titleUpdate : (Float, World) -> Model -> Model
titleUpdate (dt,w) m = setSize w <| case w.c.shoot of
  False -> m
  True  -> {m | mode <- Transition m.time}


-- compute size of space from window size only until the game starts
setSize : World -> Model -> Model
setSize wld m =
  let (sw,sh) = (toF wld.w, toF wld.h)
      w'      = sh * m.aspect  -- from window height, compute width corresponding to 4:3 ratio
  in  case (compare w' sw) of   
           LT -> {m | size <- (w' * 0.8, sh * 0.8) } -- if computed width < window width, use it
           EQ -> {m | size <- (w' * 0.8, sh * 0.8) }
           GT -> {m | size <- (sw * 0.8, sw * 0.8 / m.aspect) }
           -- otherwise (GT) compute height based on width & ratio


---- intro transition sequence ----

transitionUpdate : (Float, World) -> Float -> Model -> Model
transitionUpdate w tf m = if | m.time - tf > 0 -> {m | mode <- Playing}
                             | otherwise       -> m


---------------------------------------
--            Main game logic
---------------------------------------

scaleR = 0.006  -- rotation response scale
scaleV = 0.12   -- acceleration response scale
bulV   = 80     -- bullet velocity


gameUpdate : (Float, World) -> Model -> Model
gameUpdate w = flightControls w -- apply flight controls
            >> gun w            -- launch stuff
            >> evolveAll w      -- physics
            >> reflect          -- visual copies simulating compact space
            >> applyCollisions  -- check for collisions


flightControls : (Float, World) -> Model -> Model
flightControls (dt, wld) m =
  let me = m.me
      m' = { m | f <- wld.c.thrust }
      me1 = if wld.c.thrust 
            then { me | vx <- me.vx - (sin me.r) * dt,
                        vy <- me.vy + (cos me.r) * dt }
            else me
      me2 = case wld.c.steer of
                 SLeft   -> {me1 | vr <- me1.vr + dt}
                 SCenter -> me1
                 SRight  -> {me1 | vr <- me1.vr - dt}
  in {m' | me <- me2}


-- single-fire mode
gun : (Float, World) -> Model -> Model
gun (dt, wld) m = 
  if   wld.c.shoot && m.trigg  
  then {m | trigg <- False,
            buls  <- m.buls ++ [shootFrom m.me]}
  else {m | trigg <- not wld.c.shoot}

-- continuous fire
gun' : (Float, World) -> Model -> Model
gun' (dt, wld) m = if wld.c.shoot then {m | buls <- m.buls ++ [shootFrom m.me]} else m


-- impart momentum to bullets
shootFrom : Phys -> Phys
shootFrom ph = {ph | vx <- ph.vx - bulV*(sin ph.r),
                     vy <- ph.vy + bulV*(cos ph.r),
                     vr <- 0}

-- move according to momentum
evolveAll : (Float, World) -> Model -> Model
evolveAll w m = {m | me   <- evolve w m.me,
                     buls <- evolve w <$> m.buls}

evolve : (Float, World) -> Phys -> Phys
evolve (dt, wld) ph = {ph | x <- ph.x + ph.vx * dt * scaleV,
                            y <- ph.y + ph.vy * dt * scaleV,
                            r <- ph.r + ph.vr * dt * scaleR}


---- check for bodies outside the space and replace them ----

reflectX : V2 -> Phys -> Phys
reflectX (w,h) b = 
  if | b.x < -w -> {b | x <- b.x + w}
     | b.x >  w -> {b | x <- b.x - w}
     | otherwise    -> b


reflectY : V2 -> Phys -> Phys
reflectY (w,h) b = 
  if | b.y < -h  -> {b | y <- b.y + h}
     | b.y >  h  -> {b | y <- b.y - h}
     | otherwise -> b

reflect : Model -> Model
reflect m = let reflect' (w,h) = reflectX (w,h) >> reflectY (w,h) in
  {m | me   <- reflect' m.size m.me,
       buls <- reflect' m.size <$> m.buls }


---- collision detection ----

distance : Phys -> Phys -> Float
distance g h = let dx = g.x - h.x
                   dy = g.y - h.y in sqrt <| dx*dx + dy*dy

checkDistance : Float -> Phys -> Phys -> Bool
checkDistance delta g h = if | abs (g.x - h.x) > delta -> False
                             | otherwise -> distance g h < delta

filterN : (a -> Bool) -> List a -> List a
filterN f = List.filter (f >> not)

applyCollisions : Model -> Model
applyCollisions m = 
  let bullets = filterN (checkDistance 3.0 m.me) m.buls
      db      = List.length m.buls - (List.length bullets)
  in {m | buls <- bullets,
          life <- m.life - 10 * toF db}


---- death screen ----

deadUpdate : (Float, World) -> Model -> Model
deadUpdate w = identity
