module Destroid.Control where

import Debug exposing (watch)

import Destroid.World exposing (..)
import Destroid.Model exposing (..)
import Destroid.Utils exposing (..)
import Destroid.Params exposing (..)


updater : (Float, World) -> Model -> Model
updater w m = m |> timeStep w |> watcher |> case m.mode of
  Title        -> titleUpdate w
  Transition t -> transitionUpdate w (t + 120)
  LevelIntro t -> levelIntroUpdate w
  Playing t    -> gameUpdate  w
  Dead t       -> deadUpdate w (t + 180)


watcher : Model -> Model
watcher m = {m | time  <- Debug.watch "Time [sec/50]" m.time,
                 me    <- Debug.watch "Flight data" m.me,
                 lvl   <- Debug.watch "Level" m.lvl,
                 blink <- Debug.watch "Blink" m.blink}


timeStep : (Float,World) -> Model -> Model
timeStep (dt,w) m = {m | time <- m.time + dt,
                         dt   <- dt}

-------------------------------------------
--             Title screen
-------------------------------------------

titleUpdate : (Float, World) -> Model -> Model
titleUpdate (dt,w) m = setSize w <| case w.c.shoot of
  False -> m
  True  -> {m | mode <- Transition m.time}


-- compute size of space from window size only until the game starts
setSize : World -> Model -> Model
setSize wld m =
  let (sw,sh) = (toF wld.w, toF wld.h)
      w'      = sh * aspect  -- from window height, compute width corresponding to aspect ratio
  in  case (compare w' sw) of   
           LT -> {m | screenScale <- stageScale * w' / spaceW } -- if computed width < window width, use it
           EQ -> {m | screenScale <- stageScale * w' / spaceW }
           GT -> {m | screenScale <- stageScale * sh / spaceH }
           -- otherwise (GT) compute height based on width & aspect ratio


-------------------------------------------
--        Title transition sequence
-------------------------------------------

transitionUpdate : (Float, World) -> Float -> Model -> Model
transitionUpdate w tf m = let lev = m.lvl in
  if | m.time - tf > 0 -> prepLevelIntro m
     | otherwise       -> m

prepLevelIntro : Model -> Model
prepLevelIntro m = let lev = m.lvl in
  {m | mode <- LevelIntro m.time,
       lvl  <- {lev | ts <- emitTimes m.time (List.length lev.ls - 1),
                      tf <- m.time + 2*t_fade + (toF <| List.length lev.ls)*t_sep}}

--prepare list of asteroid emission times
emitTimes : Float -> Int -> List Float
emitTimes t n = toF >> (*) t_sep >> (+) (t + t_fade) <$> [0..n]


-------------------------------------------
--          Level intro sequence
-------------------------------------------

levelIntroUpdate : (Float, World) -> Model -> Model
levelIntroUpdate w = gameUpdate w
                  >> checkIntroExpiration
                  >> emit

checkIntroExpiration : Model -> Model
checkIntroExpiration m = case m.mode of
  LevelIntro t -> if m.time > m.lvl.tf then {m | mode <- Playing t} else m
  _            -> m

emit : Model -> Model
emit m = case (m.lvl.ls, m.lvl.ts) of
  ((a,v,s) :: xs, t :: ts) -> let lev     = m.lvl
                                  aster'  = m.lvl.xi
                                  aster'' = {aster'  | sz = s}
                                  aster   = {aster'' | vx <- -v*(sin a),
                                                       vy <-  v*(cos a)}
                              in
    if   m.time > t
    then {m | ast <- aster :: m.ast,
              lvl <- {lev | ls <- xs,
                            ts <- ts}}
    else m
  _ -> m


-------------------------------------------
--             Main game logic
-------------------------------------------

gameUpdate : (Float, World) -> Model -> Model
gameUpdate w = flightControls w -- apply flight controls
            >> gun w            -- launch stuff
            >> evolveAll w      -- physics
            >> reflect          -- visual copies simulating compact space
            >> shipBulImpacts   -- check for bullet-ship collisions
            >> bullAstImpacts   -- check for bullet-Asteroid collisions
            >> shipAstImpacts   -- check for ship-asteroid collisions
            >> expireBullets
            >> checkBlink
            >> checkDeath


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
gun (dt, wld) m = let b = shootFrom m.me in
  if   wld.c.shoot && m.trigg  
  then {m | trigg <- False,
            buls  <- m.buls ++ [{b | t0 = m.time}]}
  else {m | trigg <- not wld.c.shoot}

-- continuous fire
gun' : (Float, World) -> Model -> Model
gun' (dt, wld) m = let b = shootFrom m.me in
  if wld.c.shoot then {m | buls <- m.buls ++ [{b | t0 = m.time}]} else m


b_i = 5

-- impart momentum to bullets
shootFrom : Phys a -> Phys a
shootFrom ph = {ph | vx <- ph.vx - bulV*(sin ph.r),
                     vy <- ph.vy + bulV*(cos ph.r),
                     vr <- 0,
                     x  <- ph.x - b_i * (sin ph.r),
                     y  <- ph.y + b_i * (cos ph.r)}


-- move according to momentum
evolveAll : (Float, World) -> Model -> Model
evolveAll w m = {m | me   <- evolve w m.me,
                     buls <- evolve w <$> m.buls,
                     ast  <- evolve w <$> m.ast}

evolve : (Float, World) -> Phys a -> Phys a
evolve (dt, wld) ph = {ph | x <- ph.x + ph.vx * dt * scaleV,
                            y <- ph.y + ph.vy * dt * scaleV,
                            r <- ph.r + ph.vr * dt * scaleR}


---- check for bodies outside the space and replace them ----

reflectX : V2 -> Phys a -> Phys a
reflectX (w,h) b = 
  if | b.x < -0.5*w -> {b | x <- b.x + w}
     | b.x >  0.5*w -> {b | x <- b.x - w}
     | otherwise    -> b


reflectY : V2 -> Phys a -> Phys a
reflectY (w,h) b = 
  if | b.y < -0.5*h  -> {b | y <- b.y + h}
     | b.y >  0.5*h  -> {b | y <- b.y - h}
     | otherwise -> b

reflect : Model -> Model
reflect m = let reflect' (w,h) = reflectX (w,h) >> reflectY (w,h) in
  {m | me   <- reflect' spaceSize m.me,
       buls <- reflect' spaceSize <$> m.buls,
       ast  <- reflect' spaceSize <$> m.ast}


---- collision detection ----

distance : Phys a -> Phys b -> Float
distance g h = let dx = g.x - h.x
                   dy = g.y - h.y in sqrt <| dx*dx + dy*dy

checkDistance : Float -> Phys a -> Phys b -> Bool
checkDistance delta g h = if | abs (g.x - h.x) > delta -> False
                             | otherwise -> distance g h < delta

filterN : (a -> Bool) -> List a -> List a
filterN f = List.filter (f >> not)

-- shooting yourself
shipBulImpacts : Model -> Model
shipBulImpacts m =
  let bullets = filterN (checkDistance shipHitR m.me) m.buls
      db      = List.length m.buls - (List.length bullets)
  in  if isNothing m.blink
      then {m | buls  <- bullets,
                life  <- m.life - 10 * toF db,
                blink <- if db == 0 then Nothing else Just (m.time + t_blink)}
      else m


---- asteroids ----

astSize : ASize -> Float
astSize a = case a of
  Big    -> astSizeBig
  Medium -> astSizeMedium
  Small  -> astSizeSmall

breakUp' : Asteroid -> List Asteroid
breakUp' ph = [{ph | vy <- ph.vy + astV},
               {ph | vx <- ph.vx - astV * sin(2*pi/3),
                     vy <- ph.vy + astV * cos(2*pi/3)},
               {ph | vx <- ph.vx - astV * sin(4*pi/3),
                     vy <- ph.vy + astV * cos(4*pi/3)}]

breakUp : Asteroid -> List Asteroid
breakUp a = case a.sz of
  Big    -> {a | sz <- Medium} |> breakUp'
  Medium -> {a | sz <- Small}  |> breakUp'
  Small  -> []


checkAst : Asteroid -> List (Phys a) -> List (Phys a)
checkAst a bs = filterN (checkDistance (astSize a.sz) a) bs

checkAst_Bullet : (List Asteroid, List (Phys a)) -> (List Asteroid, List (Phys a))
checkAst_Bullet (alist, blist) = case (alist,blist) of
  ([],[]) -> ([],[])
  (la,[]) -> (la,[])
  ([],lb) -> ([],lb)
  (a::la,lb) -> let lb' = checkAst a lb
                in  case (List.length lb == List.length lb') of
                         True  -> a         ::^ checkAst_Bullet (la,lb)
                         False -> breakUp a ++^ checkAst_Bullet (la,lb')

bullAstImpacts : Model -> Model
bullAstImpacts m = let (la,lb) = checkAst_Bullet (m.ast,m.buls)
                   in  {m | ast  <- la,
                            buls <- lb}


---- asteroid-ship collision ----

checkAstDistance : Phys a -> Asteroid -> Bool
checkAstDistance ph a = checkDistance (astSize a.sz + shipHitR) ph a

shipAstImpacts : Model -> Model
shipAstImpacts m = case m.blink of
  Nothing -> if   List.any (checkAstDistance m.me) m.ast
             then {m | life  <- m.life - 10,
                       blink <- Just (m.time + t_blink)}
             else m
  Just t  -> m


---- bullet expiration ----

type alias Timed a = {a | t0 : Float}

expiration : Float -> List (Timed a) -> List (Timed a)
expiration t l = case l of
  []        -> []
  (x :: xs) -> if (x.t0 - t) > 0 then x :: xs else expiration t xs

expireBullets : Model -> Model
expireBullets m = {m | buls <- expiration (m.time - bulletLife) m.buls}


---- blink check ----

checkBlink : Model -> Model
checkBlink m = case m.blink of
  Just t  -> if m.time >= t then {m | blink <- Nothing} else m
  Nothing -> m


---- death check ----

checkDeath : Model -> Model
checkDeath m = if m.life > 0 then m else {m | mode <- Dead m.time}


---- death screen ----

deadUpdate : (Float, World) -> Float -> Model -> Model
deadUpdate w t m = if m.time < t then m else istate
