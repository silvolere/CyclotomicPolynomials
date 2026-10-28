{-# LANGUAGE InstanceSigs #-}
module Hyperbolic where

newtype Hyperbolic a = Hyperbolic (a, a) deriving Eq

toHyperbolic :: Num a => (a, a) -> Hyperbolic a
toHyperbolic (x, y) = Hyperbolic (x, y)

j :: Num a => Hyperbolic a
j = Hyperbolic (0, 1)



re :: Hyperbolic a -> a
re (Hyperbolic (x, _)) = x

hy :: Hyperbolic a -> a
hy (Hyperbolic (_, y)) = y

conjugate :: Num a => Hyperbolic a -> Hyperbolic a
conjugate (Hyperbolic (a, b)) = toHyperbolic (a, -b)

modulus :: Floating a => Hyperbolic a -> a
modulus (Hyperbolic (a, b)) = sqrt ((a * a) - (b * b))

squareModulus :: Num a => Hyperbolic a -> a
squareModulus (Hyperbolic (a, b)) = (a * a) - (b * b)

instance Num a => Num (Hyperbolic a) where
    (+) :: Num a => Hyperbolic a -> Hyperbolic a -> Hyperbolic a
    (+) (Hyperbolic (a, b)) (Hyperbolic (c, d)) = toHyperbolic (a + c, b + d)

    (*) :: Num a => Hyperbolic a -> Hyperbolic a -> Hyperbolic a
    (*) (Hyperbolic (a, b)) (Hyperbolic (c, d)) = toHyperbolic ((a * c) + (b * d), (a * d) + (b * c))

    negate :: Num a => Hyperbolic a -> Hyperbolic a
    negate (Hyperbolic (x, y)) = toHyperbolic (-x, -y)

    fromInteger :: Num a => Integer -> Hyperbolic a
    fromInteger x = toHyperbolic (fromInteger x, 0)

    abs :: Num a => Hyperbolic a -> Hyperbolic a
    abs = error "Use modulus"

    signum :: Num a => Hyperbolic a -> Hyperbolic a
    signum = undefined

instance (Fractional a) => Fractional (Hyperbolic a) where
    (/) :: Fractional a => Hyperbolic a -> Hyperbolic a -> Hyperbolic a
    (/) (Hyperbolic (a, b)) (Hyperbolic (c, d)) = Hyperbolic (((a * c) + (b * d)) / ((c * c) * (d * d))
                                                    ,((b * c) - (a * d)) / ((c * c) * (d * d)))

    fromRational :: Fractional a => Rational -> Hyperbolic a
    fromRational a = Hyperbolic (fromRational a, 0)

instance (RealFloat a) => Floating (Hyperbolic a) where
    pi :: RealFloat a => Hyperbolic a
    pi = Hyperbolic (pi, 0)

    exp :: RealFloat a => Hyperbolic a -> Hyperbolic a
    exp (Hyperbolic (x, y)) = Hyperbolic (exp x * cosh y, exp x + sinh y)

    log :: RealFloat a => Hyperbolic a -> Hyperbolic a
    log (Hyperbolic (x, y)) = Hyperbolic (0.5 * log ((x * x) - (y * y)), 2 * atanh (y / (x + sqrt ((x * x) - (y * y)))))

    sin :: RealFloat a => Hyperbolic a -> Hyperbolic a
    sin (Hyperbolic (x, y)) = Hyperbolic (sin x * cos y, cos x * sin y)

    cos :: RealFloat a => Hyperbolic a -> Hyperbolic a
    cos (Hyperbolic (x, y)) = Hyperbolic (cos x * cos y, -(sin x * sin y))

    asin :: RealFloat a => Hyperbolic a -> Hyperbolic a
    asin (Hyperbolic (x, y)) = Hyperbolic ((asin (x + y) + asin (x - y)) / 2, (asin (x + y) - asin (x - y)) / 2)

    acos :: RealFloat a => Hyperbolic a -> Hyperbolic a
    acos (Hyperbolic (x, y)) = Hyperbolic ((acos (x + y) + acos (x - y)) / 2, (acos (x + y) - acos (x - y)) / 2)

    atan :: RealFloat a => Hyperbolic a -> Hyperbolic a
    atan (Hyperbolic (x, y)) = Hyperbolic ((atan (x + y) + atan (x - y)) / 2, (atan (x + y) - atan (x - y)) / 2)

    sinh :: RealFloat a => Hyperbolic a -> Hyperbolic a
    sinh (Hyperbolic (x, y)) = Hyperbolic (sinh x * cosh y, cosh x * sinh y)

    cosh :: RealFloat a => Hyperbolic a -> Hyperbolic a
    cosh (Hyperbolic (x, y)) = Hyperbolic (cosh x * cosh y, sinh x * sinh y)

    tanh :: RealFloat a => Hyperbolic a -> Hyperbolic a
    tanh (Hyperbolic (x, y)) = Hyperbolic ((tanh (x + y) + tanh (x - y)) / 2, (tanh (x + y) - tanh (x - y)) / 2)

    asinh :: RealFloat a => Hyperbolic a -> Hyperbolic a
    asinh (Hyperbolic (x, y)) = Hyperbolic ((asinh (x + y) + asinh (x - y)) / 2, (asinh (x + y) - asinh (x - y)) / 2)

    acosh :: RealFloat a => Hyperbolic a -> Hyperbolic a
    acosh (Hyperbolic (x, y)) = Hyperbolic ((acosh (x + y) + acosh (x - y)) / 2, (acosh (x + y) - acosh (x - y)) / 2)

    atanh :: RealFloat a => Hyperbolic a -> Hyperbolic a
    atanh (Hyperbolic (x, y)) = Hyperbolic ((atanh (x + y) + atanh (x - y)) / 2, (atanh (x + y) - atanh (x - y)) / 2)

instance (Show a, Num a, Ord a) => Show (Hyperbolic a) where
    show :: Show a => Hyperbolic a -> String
    show (Hyperbolic (x, y)) = if y >= 0
                            then show x ++ " + " ++ show y ++ "j"
                            else show x ++ " - " ++ tail (show y) ++ "j"