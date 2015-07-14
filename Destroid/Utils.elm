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

(++^) : List a -> (List a, List b) -> (List a, List b)
(++^) la1 (la2,lb) = (la1 ++ la2, lb)

infixr 6 ++^

(::^) : a -> (List a, List b) -> (List a, List b)
(::^) a1 (la,lb) = (a1 :: la, lb)

infixr 6 ::^
