module Destroid.Utils where


(<$>) : (a -> b) -> List a -> List b
(<$>) = List.map

infixr 1 <$>

type alias V2 = (Float,Float)

v2scale r (x,y) = (r*x,r*y)

toF = toFloat

appendIf : Bool -> List a -> List a -> List a
appendIf b ys xs = case b of
  False -> xs
  True  -> xs ++ ys
