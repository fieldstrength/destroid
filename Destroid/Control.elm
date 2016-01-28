module Destroid.Control where

import Debug exposing (watch)

import Destroid.World  exposing (..)
import Destroid.Model  exposing (..)
import Destroid.Utils  exposing (..)
import Destroid.Params exposing (..)
import Destroid.Rand   exposing (..)


updater : (Float, World) -> Model -> Model
updater w m = m |> timeStep w |> watcher |> case m.mode of
  Title        -> titleUpdate w
  Transition t -> transitionUpdate w (t + 120)
  LevelIntro t -> levelIntroUpdate w
  Playing t    -> gameUpdate  w
  Dead t       -> deadUpdate (t + t_death) w
  Cleared t    -> clearUpdate t w


watcher : Model -> Model
watcher m = {m | time = Debug.watch "Time [sec/50]" m.time,
                 me   = Debug.watch "Flight data" m.me}


timeStep : (Float,World) -> Model -> Model
timeStep (dt,w) m = {m | time = m.time + dt,
                         dt   = dt}

-------------------------------------------
--             Title screen
-------------------------------------------

titleUpdate : (Float, World) -> Model -> Model
titleUpdate (dt,w) m = setSize w <| case w.c.shoot of
  False -> m
  True  -> {m | mode = Transition m.time}


-- compute size of space from window size only until the game starts
setSize : World -> Model -> Model
setSize wld m =
  let (sw,sh) = (toF wld.w, toF wld.h)
      w'      = sh * aspect  -- from window height, compute width corresponding to aspect ratio
  in  case (compare w' sw) of
           LT -> {m | screenScale = stageScale * w' / spaceW } -- if computed width < window width, use it
           EQ -> {m | screenScale = stageScale * w' / spaceW }
           GT -> {m | screenScale = stageScale * sh / spaceH }
           -- otherwise (GT) compute height based on width & aspect ratio


-------------------------------------------
--        Title transition sequence
-------------------------------------------

transitionUpdate : (Float, World) -> Float -> Model -> Model
transitionUpdate w tf m = let lev = m.lvl in
  if m.time - tf > 0 then prepLevelIntro m else m

prepLevelIntro : Model -> Model
prepLevelIntro m = {m | mode = LevelIntro m.time,
                        lvl  = mkLev m}

--prepare list of asteroid emission times
emitTimes : Float -> Int -> List Float
emitTimes t n = (+) (t + t_fade) << (*) t_sep << toF <$> [0..n]


mkLevList : Model -> List (Float,Float,ASize)
mkLevList m = List.map3 (,,) (genAngles (m.levnum + 1) m)
                             (genFloats 5 30 (m.levnum + 1) m)
                             (List.repeat (m.levnum + 1) Big)


mkPoint : Model -> Phys {}
mkPoint m = case (genFloats (-spaceW/2) (spaceW/2) 2 m) of
  [px,py] -> {x0 | x = px, y = py}
  _ -> Debug.crash ""


mkLev : Model -> Level
mkLev m = let l = mkLevList m in
  {ls = l,
   ts = emitTimes m.time (length l - 1),
   tf = m.time + 2 * t_fade + (toF <| length l) * t_sep,
   xi = mkPoint m}


-------------------------------------------
--          Level intro sequence
-------------------------------------------

levelIntroUpdate : (Float, World) -> Model -> Model
levelIntroUpdate w = flightControls w -- apply flight controls
                  >> gun w            -- launch stuff
                  >> evolveAll w      -- physics
                  >> reflect          -- visual copies simulating compact space
                  >> shipBulImpacts   -- check for bullet-ship collisions
                  >> bullAstImpacts   -- check for bullet-Asteroid collisions
                  >> shipAstImpacts   -- check for ship-asteroid collisions
                  >> expireBullets
                  >> checkBlink       --  (/gameupdate minus checkClear)
                  >> checkIntroExpiration
                  >> emit
--                  >> checkClear  -- uncomment when tweaking clear screen

checkIntroExpiration : Model -> Model
checkIntroExpiration m = case m.mode of
  LevelIntro t -> if m.time > m.lvl.tf then {m | mode = Playing t} else m
  _            -> m

emit : Model -> Model
emit m = case (m.lvl.ls, m.lvl.ts) of
  ((a,v,s)::xs, t::ts) -> let lev = m.lvl
                              aPos = lev.xi
                              aster = { sz = s,
                                        x  = aPos.x,
                                        y  = aPos.y,
                                        r  = 0,
                                        vx = -v*(sin a),
                                        vy =  v*(cos a),
                                        vr = 0}
                          in
    if   m.time > t
    then {m | ast = aster :: m.ast,
              lvl = {lev | ls = xs, ts = ts}}
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
            >> checkClear


flightControls : (Float, World) -> Model -> Model
flightControls (dt, wld) m =
  let me = m.me
      m' = { m | f = wld.c.thrust }
      me1 = if wld.c.thrust
            then { me | vx = me.vx - (sin me.r) * dt,
                        vy = me.vy + (cos me.r) * dt }
            else me
      me2 = case wld.c.steer of
                 SLeft   -> {me1 | vr = me1.vr + dt}
                 SCenter -> me1
                 SRight  -> {me1 | vr = me1.vr - dt}
  in {m' | me = me2}

gun : (Float, World) -> Model -> Model
gun (dt, wld) m = let b = shootFrom m.me in
  if   wld.c.shoot && m.trigg
  then {m | trigg = False,
            buls  = m.buls ++ [{ t0 = m.time,
                                 x  = b.x,
                                 y  = b.y,
                                 r  = b.r,
                                 vx = b.vx,
                                 vy = b.vy,
                                 vr = 0}]}
  else {m | trigg = not wld.c.shoot}


b_i = 5

-- impart momentum to bullets
shootFrom : Phys a -> Phys a
shootFrom ph = {ph | vx = ph.vx - bulV*(sin ph.r),
                     vy = ph.vy + bulV*(cos ph.r),
                     vr = 0,
                     x  = ph.x - b_i * (sin ph.r),
                     y  = ph.y + b_i * (cos ph.r)}


-- move according to momentum
evolveAll : (Float, World) -> Model -> Model
evolveAll w m = {m | me   = evolve w m.me,
                     buls = evolve w <$> m.buls,
                     ast  = evolve w <$> m.ast}

evolve : (Float, World) -> Phys a -> Phys a
evolve (dt, wld) ph = {ph | x = ph.x + ph.vx * dt * scaleV,
                            y = ph.y + ph.vy * dt * scaleV,
                            r = ph.r + ph.vr * dt * scaleR}


---- check for bodies outside the space and replace them ----

reflectX : V2 -> Phys a -> Phys a
reflectX (w,h) b =
  if b.x < -0.5*w then {b | x = b.x + w}
  else if b.x >  0.5*w then {b | x = b.x - w}
  else b


reflectY : V2 -> Phys a -> Phys a
reflectY (w,h) b =
  if b.y < -0.5*h then {b | y = b.y + h}
  else if b.y >  0.5*h then {b | y = b.y - h}
  else b

reflect : Model -> Model
reflect m = let reflect' (w,h) = reflectX (w,h) >> reflectY (w,h) in
  {m | me   = reflect' spaceSize m.me,
       buls = reflect' spaceSize <$> m.buls,
       ast  = reflect' spaceSize <$> m.ast}


---- collision detection ----

distance : Phys a -> Phys b -> Float
distance g h = let dx = g.x - h.x
                   dy = g.y - h.y in sqrt <| dx*dx + dy*dy

checkDistance : Float -> Phys a -> Phys b -> Bool
checkDistance delta g h = if abs (g.x - h.x) > delta then False else distance g h < delta

filterN : (a -> Bool) -> List a -> List a
filterN f = List.filter (f >> not)

-- shooting yourself
shipBulImpacts : Model -> Model
shipBulImpacts m =
  let bullets = filterN (checkDistance shipHitR m.me) m.buls
      db      = length m.buls - (length bullets)
  in  if isNothing m.blink
      then {m | buls  = bullets,
                life  = m.life - 10 * toF db,
                blink = if db == 0 then Nothing else Just (m.time + t_blink)}
      else m


---- asteroids ----

astSize : ASize -> Float
astSize a = case a of
  Big    -> astSizeBig
  Medium -> astSizeMedium
  Small  -> astSizeSmall

breakUp' : Float -> Asteroid -> List Asteroid
breakUp' r ph = [{ph | vx = ph.vx - astV * sin r,
                       vy = ph.vy + astV * cos r},
                 {ph | vx = ph.vx - astV * sin(r + 2*pi/3),
                       vy = ph.vy + astV * cos(r + 2*pi/3)},
                 {ph | vx = ph.vx - astV * sin(r + 4*pi/3),
                       vy = ph.vy + astV * cos(r + 4*pi/3)}]

breakUp : Float -> Asteroid -> List Asteroid
breakUp r a = case a.sz of
  Big    -> {a | sz = Medium} |> breakUp' r
  Medium -> {a | sz = Small}  |> breakUp' r
  Small  -> []


checkAst : Asteroid -> List (Phys a) -> List (Phys a)
checkAst a bs = filterN (checkDistance (astSize a.sz) a) bs

astScore : Asteroid -> Int
astScore a = case a.sz of
  Big    -> 2
  Medium -> 5
  Small  -> 20

checkAst_Bullet : (List Asteroid, List (Phys a), (Int,Float)) ->
                  (List Asteroid, List (Phys a), (Int,Float))
checkAst_Bullet (alist, blist, p) = case (alist,blist,p) of
  ([],[],(n,x))    -> ([],[],(n,x))
  (la,[],(n,x))    -> (la,[],(n,x))
  ([],lb,(n,x))    -> ([],lb,(n,x))
  (a::la,lb,(n,x)) -> let lb' = checkAst a lb
                      in
    case (length lb == length lb') of
         True  -> a           ::^^ checkAst_Bullet (la,lb,(n,x))
         False -> breakUp x a ++^^ checkAst_Bullet (la,lb',(astScore a + n,x))

bullAstImpacts : Model -> Model
bullAstImpacts m = let (la,lb,(s,t)) = checkAst_Bullet (m.ast,m.buls,(0,m.time))
                   in  {m | ast   = la,
                            buls  = lb,
                            score = m.score + s}


---- asteroid-ship collision ----

checkAstDistance : Phys a -> Asteroid -> Bool
checkAstDistance ph a = checkDistance (astSize a.sz + shipHitR) ph a

shipAstImpacts : Model -> Model
shipAstImpacts m = case m.blink of
  Nothing -> if   List.any (checkAstDistance m.me) m.ast
             then {m | life  = m.life - 10,
                       blink = Just (m.time + t_blink)}
             else m
  Just t  -> m


---- bullet expiration ----

type alias Timed a = {a | t0 : Float}

expiration : Float -> List (Timed a) -> List (Timed a)
expiration t l = case l of
  []        -> []
  (x :: xs) -> if (x.t0 - t) > 0 then x :: xs else expiration t xs

expireBullets : Model -> Model
expireBullets m = {m | buls = expiration (m.time - bulletLife) m.buls}


---- blink check ----

checkBlink : Model -> Model
checkBlink m = case m.blink of
  Just t  -> if m.time >= t then {m | blink = Nothing} else m
  Nothing -> m


---- death check ----

checkDeath : Model -> Model
checkDeath m = if m.life > 0 then m else {m | mode = Dead m.time}


---- completion check ----

checkClear : Model -> Model
checkClear m = if List.isEmpty m.ast
               then {m | mode  =  Cleared m.time,
                         blink = Nothing,
                         score = m.score + 400 + 100*m.levnum}
               else m


-------------------------------------------
--             Death screen
-------------------------------------------

deadUpdate : Float -> (Float, World) -> Model -> Model
deadUpdate t w m = let newhi = max m.score m.hi
                   in  if m.time >= t then {istate | hi = newhi} else
  m |> evolveAll w
    >> reflect
    >> bullAstImpacts
    >> expireBullets


-------------------------------------------
--           Level cleared screen
-------------------------------------------

clearUpdate : Float -> (Float,World) -> Model -> Model
clearUpdate t w m = if m.time >= (t + t_cleared)
                    then {m | levnum = m.levnum + 1} |> prepLevelIntro
                    else
  m |> gun w
    >> flightControls w
    >> evolveAll w
    >> reflect
    >> bullAstImpacts
    >> expireBullets
