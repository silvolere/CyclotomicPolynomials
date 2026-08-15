{-# LANGUAGE InstanceSigs #-}
module Polynomial (Polynomial,
                   listPoly,
                   toList,
                   zeroPoly,
                   lead,
                   degree,
                   polyAdd,
                   polyMult,
                   polyDivRem,
                   polyDivInt,
                   polyApply,
                   polyCompose,
                   (<.>),
                   scale,
                   (.*),
                   evaluate,
                   derive,
                   integrate,
                   defIntegrate
                  ) where

newtype Polynomial a = Polynomial [a]

listPoly :: (Num a, Eq a) => [a] -> Polynomial a -- use this, not the constructor, to eliminate trailing zeros
listPoly = Polynomial . zeroReduce

toList :: Polynomial a -> [a]
toList (Polynomial p) = p

zeroPoly :: Polynomial a
zeroPoly = Polynomial []

lead :: (Eq a, Num a) => Polynomial a -> a
lead (Polynomial p) = if p /= [] then last $ zeroReduce p else 0

degree :: (Num a, Eq a) => Polynomial a -> Int
degree (Polynomial p) = length (zeroReduce p) - 1

polyAdd :: Num a => Polynomial a -> Polynomial a -> Polynomial a
polyAdd (Polynomial p1) (Polynomial p2) = Polynomial $ zipWith (+) (extend p1) (extend p2)
    where
    extend p = take (max (length p1) (length p2)) (p ++ repeat 0)

polyMult :: Num a => Polynomial a -> Polynomial a -> Polynomial a
polyMult (Polynomial p1) (Polynomial p2) = Polynomial [sum [ p1i * p2j |
                                                      (p1i, i) <- zip p1 [0..],
                                                      (p2j, j) <- zip p2 [0..],
                                                      i + j == k ] | k <- [0..(length p1 + length p2 - 2)]]

polyDivRem :: (Eq a, Fractional a) => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
polyDivRem n d = polyDivRem' zeroPoly n
    where
    polyDivRem' q r
        | degree d > degree r = (q, r)
        | otherwise           = polyDivRem' (q + t) (r - (t * d))
        where
            t = listPoly $ replicate (degree r - degree d) 0 ++ [lead r / lead d]

polyDivInt :: (Integral a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
polyDivInt n d = polyDivInt' zeroPoly n
    where
    polyDivInt' q r
        | degree d > degree r = q
        | otherwise           = polyDivInt' (q + t) (r - (t * d))
        where
            t = listPoly $ replicate (degree r - degree d) 0 ++ [lead r `div` lead d]

-- plug some x^n in a polynomaial p(x)
polyApply :: (Num a, Eq a) => Polynomial a -> (a, Int) -> Polynomial a
polyApply (Polynomial p) (c, d) = listPoly $ zeroIntersperse $ zipWith (*) p [c ^ i | i <- ([0..] :: [Integer])]
    where
    zeroIntersperse []     = []
    zeroIntersperse (x:xs) = x : replicate (d - 1) 0 ++ zeroIntersperse xs

-- finds h(x) = g(f(x)) where g and f are polynomials
polyCompose :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
polyCompose (Polynomial g) f = sum $ zipWith (.*) g [f ^ i | i <- [0..] :: [Integer]]

-- infix polynomial composition
(<.>) :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
(<.>) = polyCompose

-- multiply all coefficients by some scalar x
scale :: (Num a, Eq a) => a -> Polynomial a -> Polynomial a
scale x = fmap (* x)

-- infix version of scale
(.*) ::  (Num a, Eq a) => a -> Polynomial a -> Polynomial a
(.*) = scale

-- plug some value in a polynomial
evaluate :: Num a => Polynomial a -> a -> a
evaluate (Polynomial p) x = ev p 0
    where
    ev [] _      = 0
    ev (c:cs) ex = c * (x ^ (ex :: Integer)) + ev cs (ex + 1)

-- derivative of polynomial with respect to x
derive :: (Num a, Eq a) => Polynomial a -> Polynomial a
derive (Polynomial p)
    | null p    = zeroPoly
    | otherwise = listPoly $ zipWith (*) (tail p) (rep 1)
    where rep n = n : rep (n + 1) -- used this instead of [1..] to get around enum typeclass lol

-- integral of p(x) with respect to x, ignore constant term +C (maybe I'll add a way to represent constants in the future)
integrate :: (Fractional a, Num a, Eq a) => Polynomial a -> Polynomial a
integrate (Polynomial p)
    | null p    = zeroPoly
    | otherwise = listPoly $ 0 : zipWith (/) p (rep 1)
    where rep n = n : rep (n + 1)

-- definite integral with bounds from a to b
defIntegrate :: (Fractional a, Eq a) => a -> a -> Polynomial a -> a
defIntegrate a b p = evaluate i b - evaluate i a
    where i = integrate p

zeroReduce :: (Num a, Eq a) => [a] -> [a]
zeroReduce xs = zeroCount xs 0
    where
    zeroCount [] c = take (length xs - c) xs
    zeroCount (y:ys) c
        | y /= 0    = zeroCount ys 0
        | otherwise = zeroCount ys (c + 1)

instance (Num a, Eq a) => Num (Polynomial a) where
    (+) :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
    (+) = polyAdd

    (*) :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
    (*) = polyMult

    negate :: (Num a, Eq a) => Polynomial a -> Polynomial a
    negate (Polynomial p) = Polynomial (map negate p)

    fromInteger :: (Num a, Eq a) => Integer -> Polynomial a
    fromInteger n = listPoly [fromInteger n]

    abs :: (Num a, Eq a) => Polynomial a -> Polynomial a
    abs = undefined

    signum :: (Num a, Eq a) => Polynomial a -> Polynomial a
    signum = undefined

instance (Num a, Eq a) => Eq (Polynomial a) where
    (==) :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Bool
    (Polynomial p1) == (Polynomial p2) = p1 == p2

instance (Num a, Eq a, Show a, Ord a) => Show (Polynomial a) where
    show :: (Num a, Eq a, Show a) => Polynomial a -> String
    show (Polynomial p) = case terms of
        [] -> "0"
        _  -> shaveOpp $ unwords $ reverse terms
        where
        terms = [showTerm c i | (c, i) <- zip p ([0..] :: [Integer]), c /= 0]
        showTerm c 0    = if c > 0 then "+ " ++ show c else "- " ++ tail (show c)
        showTerm 1 1    = "+ x"
        showTerm (-1) 1 = "- x"
        showTerm c 1    = if c > 0 then "+ " ++ show c ++ "x" else "- " ++ tail (show c) ++ "x"
        showTerm 1 i    = "+ x^" ++ show i
        showTerm (-1) i = "- x^" ++ show i
        showTerm c i    = if c > 0 then "+ " ++ show c ++ "x^" ++ show i else "- " ++ tail (show c) ++ "x^" ++ show i
        shaveOpp s
            | take 2 s == "+ " = drop 2 s
            | take 2 s == "- " = "-" ++ drop 2 s
            | otherwise        = s

instance Functor Polynomial where
    fmap :: (a -> b) -> Polynomial a -> Polynomial b
    fmap f (Polynomial p) = Polynomial (map f p)

instance Applicative Polynomial where
    pure :: a -> Polynomial a
    pure x = Polynomial [x]

    (<*>) :: Polynomial (a -> b) -> Polynomial a -> Polynomial b
    (<*>) (Polynomial f) (Polynomial p) = Polynomial (f <*> p)