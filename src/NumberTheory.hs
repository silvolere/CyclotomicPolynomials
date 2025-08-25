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
        | otherwise            = fromIntegral (firstFactor n 2) : dpf (m `div` firstFactor n 2) (firstFactor n 2)
    firstFactor m' p = if p `isFactorOf` m' then p else firstFactor m' (p + 1)

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

-- Euler's totient function, counting the number of relative primes of n up to n
totient :: (Integral a, Num b) => a -> b
totient n = 1 + totient' 2
    where
    totient' k
        | k == n       = 0
        | gcd k n == 1 = 1 + totient' (k + 1)
        | otherwise    = totient' (k + 1)

cototient :: (Integral a, Num b) => a -> b
cototient n = cototient' 2
    where
    cototient' k
        | k == n       = 1
        | gcd k n == 1 = cototient' (k + 1)
        | otherwise    = 1 + cototient' (k + 1)

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

--isWeird

isHighlyComposite :: Integral a => a -> Bool
isHighlyComposite n = all ((< divisorCount n) . divisorCount) [1..n - 1]

--isSuperiorHighlyComposite

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
isBsmooth b n = b >= last (distinctPrimeFactors n)