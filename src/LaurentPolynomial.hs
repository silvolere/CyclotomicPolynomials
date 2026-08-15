{-# LANGUAGE InstanceSigs #-}
module LaurentPolynomial (LPolynomial,
                          listLPoly,
                          degree,
                          highestPower,
                          lowestPower
                         ) where

newtype LPolynomial a = LPolynomial ([a], Int)

-- Int in second tuple position represents the smallest exponent value; if 0, it is the same as a regular polynomial
listLPoly :: (Num a, Eq a) => ([a], Int) -> LPolynomial a
listLPoly ([], _) = LPolynomial ([], 0)
listLPoly (xs, m) = LPolynomial (zeroReduce xs, m)

-- the degree of a Laurent polynomial is the largest exponent minus the smallest
degree :: (Num a, Eq a) => LPolynomial a -> Int
degree (LPolynomial ([], _)) = 0
degree (LPolynomial (xs, _)) = length xs - 1

highestPower :: LPolynomial a -> Int
highestPower (LPolynomial (xs, m)) = length xs - 1 + m

lowestPower :: LPolynomial a -> Int
lowestPower (LPolynomial (_, m)) = m

zeroReduce :: (Num a, Eq a) => [a] -> [a]
zeroReduce xs = zeroCount xs 0
    where
    zeroCount [] c = take (length xs - c) xs
    zeroCount (y:ys) c
        | y /= 0    = zeroCount ys 0
        | otherwise = zeroCount ys (c + 1)

lPolyAdd :: Num a => LPolynomial a -> LPolynomial a -> LPolynomial a
lPolyAdd (LPolynomial (x, m1)) (LPolynomial (y, m2)) = LPolynomial (uncurry (zipWith (+)) zrs, min m1 m2)
    where
    zrs
        | m1 > m2 && s1 < s2 = (replicate (abs (m1 - m2)) 0 ++ x ++ replicate (abs (s1 - s2)) 0, y)
        | m1 > m2            = (replicate (abs (m1 - m2)) 0 ++ x, y ++ replicate (abs (s1 - s2)) 0)
        | s1 < s2            = (x ++ replicate (abs (s1 - s2)) 0, replicate (abs (m1 - m2)) 0 ++ y)
        | otherwise          = (x, replicate (abs (m1 - m2)) 0 ++ y ++ replicate (abs (s1 - s2)) 0)
    s1 = m1 + length x
    s2 = m2 + length y

lPolyMult :: Num a => LPolynomial a -> LPolynomial a -> LPolynomial a
lPolyMult (LPolynomial (x, m1)) (LPolynomial (y, m2)) = LPolynomial ([sum [exi * eyj |
                                                                    (exi, i) <- zip ex [0..],
                                                                    (eyj, j) <- zip ey [0..],
                                                                    i + j == k ] | k <- [0..(length ex + length ey - 2)]], m1 * m2)
    where
    (ex, ey)
        | m1 > m2   = (replicate (abs (m1 - m2)) 0 ++ x, y)
        | otherwise = (x, replicate (abs (m1 - m2)) 0 ++ y)


instance (Num a, Eq a) => Num (LPolynomial a) where
    (+) :: (Num a, Eq a) => LPolynomial a -> LPolynomial a -> LPolynomial a
    (+) = lPolyAdd

    (*) :: (Num a, Eq a) => LPolynomial a -> LPolynomial a -> LPolynomial a
    (*) = lPolyMult

    negate :: (Num a, Eq a) => LPolynomial a -> LPolynomial a
    negate (LPolynomial (x, m)) = LPolynomial (map negate x, m)

    fromInteger :: (Num a, Eq a) => Integer -> LPolynomial a
    fromInteger n = listLPoly ([fromInteger n], 0)

    abs :: (Num a, Eq a) => LPolynomial a -> LPolynomial a
    abs = undefined

    signum :: (Num a, Eq a) => LPolynomial a -> LPolynomial a
    signum = undefined
    
instance (Num a, Eq a) => Eq (LPolynomial a) where
    (==) :: (Num a, Eq a) => LPolynomial a -> LPolynomial a -> Bool
    (==) (LPolynomial (x, m1)) (LPolynomial (y, m2)) = x == y && m1 == m2

instance (Num a, Eq a, Ord a, Show a) => Show (LPolynomial a) where
    show :: (Num a, Show a, Eq a) => LPolynomial a -> String
    show (LPolynomial (x, m)) = case terms of
        [] -> "0"
        _  -> shaveOpp $ unwords $ reverse terms
        where
        terms = [showTerm c i | (c, i) <- zip x [m..], c /= 0]
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