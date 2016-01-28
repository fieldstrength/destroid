module Destroid.Utils where


(<$>) : (a -> b) -> List a -> List b
(<$>) = List.map

infixr 1 <$>

type alias V2 = (Float,Float)

v2scale r (x,y) = (r*x,r*y)

toF = toFloat

length = List.length

zipWith = List.map2

appendIf : Bool -> List a -> List a -> List a
appendIf b ys xs = case b of
  False -> xs
  True  -> xs ++ ys


(++^) : List a -> (List a, b) -> (List a, b)
(++^) la1 (la2,lb) = (la1 ++ la2, lb)

(::^) : a -> (List a, b) -> (List a, b)
(::^) a1 (la,lb) = (a1 :: la, lb)

(++^^) : List a -> (List a,b,c) -> (List a,b,c)
(++^^) la1 (la2,x,y) = (la1 ++ la2,x,y)

(::^^) : a -> (List a,b,c) -> (List a,b,c)
(::^^) a1 (la,x,y) = (a1 :: la,x,y)


infixr 6 ++^
infixr 6 ::^
infixr 6 ++^^
infixr 6 ::^^


isJust : Maybe a -> Bool
isJust m = case m of
  Just x  -> True
  Nothing -> False

isNothing : Maybe a -> Bool
isNothing = isJust >> not

fromJust : Maybe a -> a
fromJust mx = case mx of
  Just x -> x
  Nothing -> Debug.crash "FromJust: your Maybe value is not a 'Just'"
