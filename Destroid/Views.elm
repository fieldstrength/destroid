module Destroid.Views where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug exposing (watch)

import Text exposing (fromString)

import Destroid.Utils exposing (..)
import Destroid.Model exposing (..)
import Destroid.World exposing (World)
import Destroid.Params exposing (..)
import Destroid.Debug exposing (debug_panel)


view : World -> Model -> Element
view w m = case m.mode of
  Title        -> titleView w m
  Transition t -> transitionView w t m
  LevelIntro t -> levelIntroView w t m
  Playing t    -> gameView w m
  Dead t       -> deadView w m


---------------------------------------
--            Title view
---------------------------------------

titleView : World -> Model -> Element
titleView wld m = let (w,h)   = stage m
                      (cw,ch) = (wld.w, wld.h)
                  in  collage cw ch <|
  [rect (toF cw) (toF ch) |> filled titleBG,
   shipIntroAnim 0 m,
   image 700 150 "../img/Logo.png"       |> toForm |> move (0,0.4*h), -- original 1400 x 300
   image 400 40  "../img/PressStart.png" |> toForm |> move (0,-0.2*h) |> alpha (sq <| cos (m.time/45)), -- 800 x 80
   image 600 100 "../img/Controls.png"   |> toForm |> move (0,-0.45*h)] -- 1200 x 200

shipIntroAnim : Float -> Model -> Form
shipIntroAnim dr m = let (w,h) = stage m
                         scale = hyper (4 - dr) * shipSize
                     in
  [booster         |> List.map (v2scale scale) |> polygon |> filled boostercolor |> alpha (dr/4),
   booster |> refl |> List.map (v2scale scale) |> polygon |> filled boostercolor |> alpha (dr/4),
   shipdesign      |> List.map (v2scale scale) |> polygon |> filled shipcolor]
     |> group

hyper x = sqrt(1 + x*x)

sq x = x*x


---------------------------------------
--          Transition view
---------------------------------------

transitionView : World -> Float -> Model -> Element
transitionView wld t0 m =
  let (w,h) = (wld.w, wld.h)
      alph  = Debug.watch "Intro animation alpha" <| (m.time - t0) / 120
  in  collage w h <|
        [rect (toF w) (toF h) |> filled titleBG,
         rect (toF w) (toF h) |> filled gameBG |> alpha alph,
         shipIntroAnim (alph*4) m]


---------------------------------------
--            Level Intro view
---------------------------------------

levelIntroView : World -> Float -> Model -> Element
levelIntroView wld t m =
  let alph    = fadeInOut t (t+t_fade) (m.lvl.tf-t_fade) m.lvl.tf m.time
      (cw,ch) = (wld.w, wld.h)
  in  collage cw ch <|
           (filled gameBG <| rect (toF cw) (toF ch)) --background
        :: introRenderGroups alph m
        ++ lifeBar wld m
        ++ (if debug then [debug_panel (cw,ch) m.dt] else [])


fade : Float -> Float -> Float -> Float
fade ti tf t = (t - ti) / (tf - ti)

fadeInOut : Float -> Float -> Float -> Float -> Float -> Float
fadeInOut t1 t2 t3 t4 t =
  if | t1 < t && t <= t2 -> fade t1 t2 t
     | t2 < t && t <= t3 -> 1
     | t3 < t && t <= t4 -> fade t4 t3 t
     | otherwise         -> 0


introRenderers : Float -> Model -> List (Model -> Form)
introRenderers f m = wormhole f
                  :: renderers m
                  ++ [wormhole (f * 0.4)]

introRenderGroups : Float -> Model -> List Form
introRenderGroups f m = render m (introRenderers f m)


---- wormhole ----

wormhole : Float -> Model -> Form
wormhole f m = whform m |> alpha f

whform : Model -> Form
whform m = gradient (whgrad 100) (circle <| 200) |> position m m.lvl.xi

whgrad : Float -> Gradient
whgrad n = radial (0,0) (0.2*n) (0,0) n
    [ (0, rgb  0 0 0),
      (1, rgba 0 0 0 0) ]


---------------------------------------
--            Game view
---------------------------------------

gameView : World -> Model -> Element
gameView wld m =
  let (cw,ch) = (wld.w, wld.h)
  in  collage cw ch <|
           (filled gameBG <| rect (toF cw) (toF ch)) --background
        :: renderGroups m
        ++ lifeBar wld m
        ++ (if debug then [debug_panel (cw,ch) m.dt] else [])


renderGroups : Model -> List Form
renderGroups m = render m (renderers m)

renderers : Model -> List (Model -> Form)
renderers m = ship
           :: (bullet   <$> m.buls)
           ++ (asteroid <$> m.ast)

render : Model -> List (Model -> Form) -> List Form
render m l = case l of
  []        -> []
  (f :: fs) -> duplicate m (f m) ++ render m fs


duplicate : Model -> Form -> List Form
duplicate m f = flip move f <$> copies (stage m)

copies : V2 -> List V2
copies (x,y) =
  [
    (-x, -y),
    (-x, 0),
    (-x, y),
    (0, -y),
    (0, 0),
    (0, y),
    (x, -y),
    (x, 0),
    (x, y)
  ]


position : Model -> Phys a -> Form -> Form
position m ph = move (screenCoords m (ph.x,ph.y)) >> rotate ph.r


---- the ship ----

ship : Model -> Form
ship m =
  let sh =
    [booster         |> List.map (v2scale shipSize) |> polygon |> filled boostercolor,
     booster |> refl |> List.map (v2scale shipSize) |> polygon |> filled boostercolor,
     shipdesign      |> List.map (v2scale shipSize) |> polygon |> filled shipcolor]
       |> appendIf (m.f) [flame |> refl |> List.map (v2scale shipSize) |> polygon |> boostgrad shipSize,
                          flame |>         List.map (v2scale shipSize) |> polygon |> boostgrad shipSize]
       |> group
       |> position m m.me
  in if isNothing m.blink then sh else sh |> alpha 0.4


refl = List.map (\(x,y) -> (-x,y))

shipdesign = [(0.00,4.40),
              (0.26,3.84),
              (0.82,0.8),
              (2.48,-0.3),
              (2.48,-1.2),
              (0.86,-1.0),
              (0.00,-1.9),
              (-0.86,-1.0),
              (-2.48,-1.2),
              (-2.48,-0.3),
              (-0.82,0.8),
              (-0.26,3.84)]

booster = [(0.6,-0.9),
           (0.80,-1.3),
           (1.30,-1.3),
           (1.5,-0.9)]

flame = [(1.30,-1.3),
         (1.40,-1.8),
         (1.40,-2.5),
         (1.30,-3.0),
         (1.25,-3.5),
          
         (1.05,-3.9),
          
         (0.85,-3.5),
         (0.80,-3.0),
         (0.70,-2.5),
         (0.70,-1.8),
         (0.80,-1.3)]

boostgrad s = gradient <|
  linear (0,-s) (0,-3*s) [(0,  white),
                          (0.4,rgba 118 169 245 0.9),
                          (1,  rgba 115 75  141 0.5)]


---- bullets ----

bullet : Phys a -> Model -> Form
bullet ph m = let s = bulletSize
                  n = 16 in
  polygon [(-s,0),(-s,n*s),(s,n*s),(s,0)] 
    |> filled bulletcolor
    |> position m ph


---- asteroids ----

asteroid : Asteroid -> Model -> Form
asteroid a m = let size = (case a.sz of
                                Big    -> astSizeBig
                                Medium -> astSizeMedium
                                Small  -> astSizeSmall)
               in
  circle size |> outlined (solid white) |> position m a


---- game data displays ----

-- Lifebar is 140 x 8, positioned 20 px from the top-left screen corner
lifeBar : World -> Model -> List Form
lifeBar wld m = let life  = m.life * 1.4
                    (w,h) = (toF wld.w, toF wld.h)
                in
  move (-0.5*w + 90,0.5*h - 24) <$> [filled lifebarcolor (rect life 8) |> move ((life-140)*0.5,0),
                                     outlined (solid white) (rect 140 8)]


---------------------------------------
--            Dead view
---------------------------------------

deadView : World -> Model -> Element
deadView w m = leftAligned (fromString "YOU'RE DEAD")
