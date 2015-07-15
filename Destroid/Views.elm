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
import Destroid.Debug exposing (debug_info)


gameBG       = rgb  31  42  62
titleBG      = rgb  45  61  91
shipcolor    = rgb  225 250 213
boostercolor = rgb  169 172 146
bulletcolor  = rgb  247 104 147
lifebarcolor = rgba 214 102 220 0.7


view : World -> Model -> Element
view w m = case m.mode of
  Playing      -> gameView w m
  Title        -> titleView w m
  Transition t -> transitionView w t m
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
--            Game view
---------------------------------------

gameView : World -> Model -> Element
gameView wld m = let (cw,ch) = (wld.w, wld.h)
                 in  collage cw ch <|
  [filled gameBG <| rect (toF cw) (toF ch)] --background
     ++ bullets m
     ++ ships m
     ++ List.concat (asteroids m <$> m.ast)
     ++ lifeBar wld m
     ++ (if debug then [debug_info m.dt
                          |> color orange
                          |> width 300
                          |> toForm
                          |> move (160 - 0.5 * toF cw,30 - 0.5 * toF ch)]
                  else [])


screenCoords : Model -> (Float,Float) -> (Float,Float)
screenCoords m = v2scale m.screenScale

stage : Model -> (Float,Float)
stage m = screenCoords m spaceSize

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

duplicate : Model -> Form -> List Form
duplicate m f = flip move f <$> copies (stage m)


position : Model -> Phys a -> Form -> Form
position m ph = move (screenCoords m (ph.x,ph.y)) >> rotate ph.r


---- the ship ----

shipdesign = [(0.00,4.40),   -- 990, 660
              (0.26,3.84),
              (0.82,0.8),
              (2.48,-0.3),
              (2.48,-1.2),  -- 1234, 1218
              (0.86,-1.0),
              (0.00,-1.9),  -- 990, 1280
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


ship : Model -> Form
ship m =
   [booster         |> List.map (v2scale shipSize) |> polygon |> filled boostercolor,
    booster |> refl |> List.map (v2scale shipSize) |> polygon |> filled boostercolor,
    shipdesign      |> List.map (v2scale shipSize) |> polygon |> filled shipcolor]
      |> appendIf (m.f) [flame |> refl |> List.map (v2scale shipSize) |> polygon |> grad shipSize,
                         flame |>         List.map (v2scale shipSize) |> polygon |> grad shipSize]
      |> group
      |> position m m.me

refl = List.map (\(x,y) -> (-x,y))

grad s = gradient <| 
  linear (0,-s) (0,-3*s) [(0,  white),
                          (0.4,rgba 118 169 245 0.9),
                          (1,  rgba 115 75 141 0.5)]


ships : Model -> List Form
ships m = duplicate m (ship m)


---- bullets ----

bullet : Model -> V2 -> Phys a -> Form
bullet m (w,h) ph = let s = bulletSize
                        n = 16 in
  polygon [(-s,0),(-s,n*s),(s,n*s),(s,0)] 
    |> filled bulletcolor
    |> position m ph


bullets : Model -> List Form
bullets m = duplicate m <$> bullet m (stage m) <$> m.buls |> List.concat


---- game data displays ----

-- Lifebar is 140 x 8, positioned 20 px from the top-left screen corner
lifeBar : World -> Model -> List Form
lifeBar wld m = let life  = m.life * 1.4
                    (w,h) = (toF wld.w, toF wld.h)
                in
  move (-0.5*w + 90,0.5*h - 24) <$> [filled lifebarcolor (rect life 8) |> move ((life-140)*0.5,0),
                                     outlined (solid white) (rect 140 8)]

---- asteroids ----

asteroid : Model -> Asteroid -> Form
asteroid m a = let size = (case a.sz of
                                Big    -> astSizeBig
                                Medium -> astSizeMedium
                                Small  -> astSizeSmall)
               in
  circle size |> filled white |> position m a

asteroids : Model -> Asteroid -> List Form
asteroids m a = duplicate m (asteroid m a)

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
--            Dead view
---------------------------------------

deadView : World -> Model -> Element
deadView w m = leftAligned (fromString "YOU'RE DEAD")
