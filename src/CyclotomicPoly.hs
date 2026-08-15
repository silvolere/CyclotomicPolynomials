import Polynomial
import NumberTheory
import Complex

unityRoots :: RealFloat a => Int -> [Complex a]
unityRoots n
    | n < 1     = []
    | otherwise = cc n
    where
        x    = Complex (cos (2 * pi / fromIntegral n), sin (2 * pi / fromIntegral n))
        cc 0 = []
        cc m = x ^ (n - m) : cc (m - 1)

cycloPolyGen :: (Integral a) => Int -> Polynomial a
cycloPolyGen n
    | n == 1                    = listPoly [-1, 1]
    | isPrime n                 = listPoly (replicate n 1)
    | even n && odd (n `div` 2) = polyApply (cycloPolyGen (n `div` 2)) (-1, 1)
    | isPrimePower n            = polyApply (cycloPolyGen (head $ primeFactors n)) (1, head (primeFactors n) ^ (length (primeFactors n :: [Integer]) - 1))
    | otherwise                 = polyDivInt (listPoly $ -1 : replicate (n - 1) 0 ++ [1]) (product [cycloPolyGen d | d <- init (factors n)])