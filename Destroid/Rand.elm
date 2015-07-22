module Destroid.Rand where

import Random exposing (..)

import Destroid.Model exposing (Model)
import Destroid.Utils exposing ((<$>))


type alias IntS = (Int,Seed)

type alias FlS = (Float,Seed)


gen : Int -> Generator a -> (a,Seed) -> List (a,Seed)
gen l g (x,s) = let yz = generate g s in
  if | l <  1 -> []
     | l >= 1 -> yz :: gen (l-1) g yz


genI : Int -> Int -> Int -> IntS -> List IntS
genI n m l s = gen l (int n m) s

genF : Float -> Float -> Int -> FlS -> List FlS
genF n m l s = gen l (float n m) s


genInts : Int -> Int -> Int -> Model -> List Int
genInts x y l m = fst <$> genI x y l (0, initialSeed <| floor m.time)

genFloats : Float -> Float -> Int -> Model -> List Float
genFloats x y l m = fst <$> genF x y l (0, initialSeed <| floor m.time)


genInt : Int -> Int -> Model -> Int
genInt j k m = fst <| generate (int j k) (initialSeed <| floor m.time)

genFloat : Float -> Float -> Model -> Float
genFloat x y m = fst <| generate (float x y) (initialSeed <| floor m.time)


genAngle : Model -> Float
genAngle = genFloat 0 (2*pi)

genAngles : Int -> Model -> List Float
genAngles = genFloats 0 (2*pi)