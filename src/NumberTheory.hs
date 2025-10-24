{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module NumberTheory where

isFactorOf :: Integral a => a -> a -> Bool
isFactorOf a n = n `mod` a == 0

primes :: Num a => [a]
primes = sieve [2..]
    where
    sieve (p:ns) = fromIntegral p : sieve [n | n <- ns, not $ p `isFactorOf` n]

-- list of all primes less than or equal to some real number x
primesBefore :: (Integral a, RealFrac b) => b -> [a]
primesBefore x = if x >= 2 then sieve [2..floor x] else []
    where
    sieve []     = []
    sieve (p:ns) = p : sieve [n | n <- ns, not $ p `isFactorOf` n]

-- number of primes less than or equal to some real number x 
primeCount :: (Num a, RealFrac b) => b -> a
primeCount x = if x >= 2 then sieveCount 0 [2..floor x] else 0
    where
    sieveCount acc []     = acc
    sieveCount acc (p:ns) = sieveCount (acc + 1) [n | n <- ns, not $ p `isFactorOf` n]

-- integer square root
isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

isPrime :: Integral a => a -> Bool
isPrime n = (abs n > 1) && null [a | a <- [2..isqrt (abs n)], a `isFactorOf` abs n]

isComposite :: Integral a => a -> Bool
isComposite n = (abs n > 3) && [] /= [a | a <- [2..isqrt (abs n)], a `isFactorOf` abs n]

primeFactors :: (Integral a, Num b) => a -> [b]
primeFactors n
    | n < 2     = []
    | isPrime n = [fromIntegral n]
    | otherwise = fromIntegral (firstFactor n 2) : primeFactors (n `div` firstFactor n 2)
    where
    firstFactor m p = if p `isFactorOf` m then p else firstFactor m (p + 1)

distinctPrimeFactors :: (Integral a, Num b) => a -> [b]
distinctPrimeFactors n = dpf n 1
    where
    dpf m l
        | m < 2                = []
        | isPrime m && m == l  = []
        | isPrime m            = [fromIntegral m]
        | firstFactor m 2 == l = dpf (m `div` l) l
        | otherwise            = fromIntegral (firstFactor m 2) : dpf (m `div` firstFactor m 2) (firstFactor m 2)
    firstFactor m' p = if p `isFactorOf` m' then p else firstFactor m' (p + 1)

-- lists all distinct prime factors and their multiplicity
primePowerFactors :: (Integral a, Num b, Eq b) => a -> [(b, a)]
primePowerFactors n = ppf (0, 0) (primeFactors n)
    where
    ppf (l, e) [] = [(l, e)]
    ppf (l, e) (p:ps)
        | l == p = ppf (l, e + 1) ps
        | l == 0 = ppf (p, 1) ps
        | otherwise = (l, e) : ppf (p, 1) ps

factors :: (Integral a, Num b) => a -> [b]
factors n
    | n == 1    = [1]
    | n == 2    = [1, 2]
    | otherwise = [fromIntegral k | k <- [1..n `div` 2 + 1], k `isFactorOf` n] ++ [fromIntegral n]

properFactors :: (Integral a, Num b) => a -> [b]
properFactors n
    | n < 2     = []
    | n == 2    = [1]
    | otherwise = [fromIntegral k | k <- [1..n `div` 2 + 1], k `isFactorOf` n]

-- finds if a number is twice a prime
is2prime :: Integral a => a -> Bool
is2prime n = even n && isPrime (n `div` 2)

isPrimePower :: Integral a => a -> Bool
isPrimePower n = n >= 2 && all (== head (primeFactors n)) (primeFactors n)

-- for some positive integers a and b, finds some integers x and y s.t. ax + by = gcd(a,b), returns x, y, gcd(a,b)
extendGCD :: (Integral t, Num a, Num b, Num c) => t -> t -> (a, b, c)
extendGCD a b = if b > a then egcd b a (b `div` a) 0 1 1 0 else egcd a b (a `div` b) 1 0 0 1
    where
    egcd c d q x1 y1 x2 y2
        | d == 0    = (fromIntegral x1, fromIntegral y1, fromIntegral c)
        | otherwise = egcd d (c - q * d) (d `div` (c - q * d)) x2 y2 (x1 - (q * x2)) (y1 - (q * y2))

-- order of a mod p, or the smallest positive exponent e s.t. a^e === 1 mod p
-- p must be a prime
orderMod :: (Integral a, Num b) => a -> a -> b
orderMod a p = fromIntegral $ head $ filter ((== 1) . (`mod` p) . (a ^)) [1..p - 1]

-- checks if a is a primitive root mod p, or the order of a mod p equals p - 1
-- a prime p has phi(p - 1) primitive roots, where phi is the totient function
isPrimitiveRoot :: Integral a => a -> a -> Bool
isPrimitiveRoot a p = orderMod a p == p - 1

-- lists all the primitive roots for some prime p
primitiveRoots :: (Integral a, Num b) => a -> [b]
primitiveRoots p = map fromIntegral $ filter (`isPrimitiveRoot` p) [1..p - 1]

-- first prime omega function, counts distinct prime factors
littleOmega :: (Integral a, Num b) => a -> b
littleOmega = fromIntegral . length . distinctPrimeFactors

-- second prime omega function, counts prime factors with multiplicity
bigOmega :: (Integral a, Num b) => a -> b
bigOmega = fromIntegral . length . primeFactors

-- Liouville lambda function, 1 for even number of prime factors, -1 otherwise
liouville :: (Integral a, Num b) => a -> b
liouville n = if (even . bigOmega) n then 1 else -1

-- Mobius function: 1 if n = 1, (-1)^k if n factors into k distinct primes, 0 if some square > 1 divides n
mobius :: (Integral a, Num b) => a -> b
mobius n = if littleOmega n == bigOmega n then liouville n else 0

-- Mertens function (I ain't explaining allat)
mertens :: (Integral a, Num b) => a -> b
mertens n = sum $ map mobius [1..n]

-- Dedekind psi function 
dedekind :: (Integral a, Num b) => a -> b
dedekind n
    | n < 1     = undefined
    | n == 1    = 1
    | otherwise = fromIntegral $ product $ map (\(p, k) -> (p + 1) * (p ^ (k - 1))) (primePowerFactors n)

-- Euler's totient function, counting the number of relative primes of n up to n
totient :: (Integral a, Num b) => a -> b
totient 1 = 1
totient n
    | n < 1 = undefined
    | otherwise = fromIntegral $ product $ map totient' (primePowerFactors n)
    where
    totient' (p, k)
        | k == 1    = p - 1
        | p == 2    = p ^ (k - 1)
        | otherwise = (p ^ k) - (p ^ (k - 1))

cototient :: (Integral a, Num b) => a -> b
cototient n = fromIntegral n - totient n

-- finds all numbers n s.t. phi(n) = m
inverseTotient :: (Integral a, Num b) => a -> [b]
inverseTotient m
    | m < 1     = []
    | m == 1    = [1, 2]
    | odd m     = []
    | otherwise = [fromIntegral n | n <- [(m + 1)..(2 * (m ^ 2))], m == totient n]

-- Dirichlet inverse of totient 
dirInvTotient :: (Integral a, Num b) => a -> b
dirInvTotient n = fromIntegral $ sum [d * mobius d | d <- factors n]

-- Jordan's totient funciton
jTotient :: (Integral a, Num b, Eq b) => a -> a -> b
jTotient k n
    | n == 1    = 1
    | otherwise = product (map (\(a, b) -> a ^ ((b - 1) * k)) (primePowerFactors n)) * product (map ((+ (-1)) . (^ k)) (distinctPrimeFactors n))

-- Carmichael Function
carmichael :: (Integral a, Num b) => a -> b
carmichael n
    | n < 1 = undefined
    | n == 1 = 1
    | otherwise = fromIntegral $ foldr (lcm . carmichael') 1 (primePowerFactors n)
    where
    carmichael' (p, k)
        | k == 1    = p - 1
        | p == 2    = p ^ (k - 2)
        | otherwise = (p - 1) * (p ^ (k - 1))

-- Legendre symbol: 1 if n is a quadradic residue mod p, -1 for quadradic nonresidue, 0 for a === 0.
-- It becomes the Jacobi symbol when p is not prime (but still odd), though all of the computation
-- works the same
legendre :: Integral a => a -> a -> a
legendre n p
    | n == 0                                       = 0
    | p == 2                                       = n `mod` p
    | n == 1                                       = 1
    | n == p - 1 && p `mod` 4 == 1                 = 1
    | n == p - 1                                   = -1
    | n == 2 && (p `mod` 8 == 1 || p `mod` 8 == 7) = 1
    | n == 2                                       = -1
    | n >= p                                       = legendre (n `mod` p) p
    | even n                                       = legendre 2 p * legendre (n `div` 2) p
    | p `mod` 4 == 1 || n `mod` 4 == 1             = legendre (p `mod` n) n
    | p `mod` 4 == 3 && n `mod` 4 == 3             = -legendre (p `mod` n) n

-- sum of proper divisors, only works on strictly positive integers
aliquotSum :: (Integral a, Num b) => a -> b
aliquotSum n
    | n < 1     = undefined
    | n == 1    = 0
    | otherwise = fromIntegral $ sum $ properFactors n

-- sum of all positive divisors
divisorSum :: (Integral a, Num b) => a -> b
divisorSum n = if n > 0 then (sum . factors) n else undefined

divisorCount :: (Integral a, Num b) => a -> b
divisorCount = fromIntegral . length . factors

isSquareFree :: Integral a => a -> Bool
isSquareFree = unique 0 . primeFactors
    where
    unique _ [] = True
    unique l (x:xs) = (l /= x) && unique x xs

isDeficient :: Integral a => a -> Bool
isDeficient n = n > aliquotSum n

isAbundant :: Integral a => a -> Bool
isAbundant n = n < aliquotSum n

isPrimitiveAbundant :: Integral a => a -> Bool
isPrimitiveAbundant n = all isDeficient (drop 1 $ factors n)

isHighlyAbundant :: Integral a => a -> Bool
isHighlyAbundant n
    | n < 1     = undefined
    | otherwise = all ((< divisorSum n) . divisorSum) [1..n]

isSuperabundant :: Integral a => a -> Bool
isSuperabundant  n
    | n < 1     = undefined
    | otherwise = all ((< (divisorSum n / fromIntegral n)) . divSumOverM) [1..n - 1]
    where
    divSumOverM m = divisorSum m / fromIntegral m

--isColossallyAbundant

isPerfect :: Integral a => a -> Bool
isPerfect n = n == aliquotSum n

isSemiperfect :: Integral a => a -> Bool -- lmao this is just subset sum
isSemiperfect n = subsetSum n (length pfn - 1)
    where
    pfn = properFactors n
    subsetSum k l
        | k == 0         = True
        | k < 0 || l < 0 = False
        | otherwise      = subsetSum k (l - 1) || subsetSum (k - (pfn !! l)) (l - 1)

isPerfectTotient :: Integral a => a -> Bool
isPerfectTotient n = n == ipt n
    where
    ipt 1 = 0
    ipt k = totient k + ipt (totient k)

-- might want to make this more efficient
isWeird :: Integral a => a -> Bool
isWeird n = isAbundant n && not (isSemiperfect n)

isUnusual :: (Integral a) => a -> Bool
isUnusual n = (sqrt . fromIntegral) n < last (primeFactors n)

isHighlyComposite :: Integral a => a -> Bool
isHighlyComposite n = all ((< divisorCount n) . divisorCount) [1..n - 1]

--isSuperiorHighlyComposite :: Integral a => a -> Bool -- this is hard :(

isPowerful :: Integral a => a -> Bool
isPowerful n = dupCheck $ primeFactors n
    where
    dupCheck []      = True
    dupCheck [_]     = False
    dupCheck (p:ps)
        | p == head ps = dupCheck (filter (p /=) ps)
        | otherwise    = False

-- prime factors of n are less than or equal to b
isBsmooth :: Integral a => a -> a -> Bool
isBsmooth b n = (abs n <= 2) || (b >= last (primeFactors n))


-- same as above but with prime powers
isBpowerSmooth :: Integral a => a -> a -> Bool
isBpowerSmooth b n = all ((<= b) . uncurry (^)) $ primePowerFactors n

-- analogous to B-smooth but for greater than or equal
isKrough :: Integral a => a -> a -> Bool
isKrough k n = (abs k > 1) || (k <= head (primeFactors n))

-- ^^^
isKpowerRough :: Integral a => a -> a -> Bool
isKpowerRough k n = all ((>= k) . uncurry (^)) $ primePowerFactors n

-- generates the nth primitive pythagorean triple via (a,b,c) = (u^2 - v^2, 2uv, u^2 + v^2) for some integers u,v
pythagTriple :: (Num a, Num b, Num c) => Int -> (a, b, c)
pythagTriple n = [(fromIntegral ((u ^ 2) - (v ^ 2)), fromIntegral (2 * u * v), fromIntegral ((u ^ 2) + (v ^ 2))) | u <- [2..], v <- [1..u - 1], odd (u + v)] !! (n - 1)