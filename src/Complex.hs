{-# LANGUAGE InstanceSigs #-}
module Complex where

newtype Complex a = Complex (a, a) deriving Eq

toComplex :: Num a => (a, a) -> Complex a -- idk if I need this, just use the constructor or something
toComplex (x, y) = Complex (x, y)

i :: Num a => Complex a
i = Complex (0, 1)

re :: Complex a -> a
re (Complex (x, _)) = x

im :: Complex a -> a
im (Complex (_, y)) = y

conjugate :: Num a => Complex a -> Complex a
conjugate (Complex (a, b)) = toComplex (a, -b)

modulus :: Floating a => Complex a -> a
modulus (Complex (a, b)) = sqrt ((a * a) + (b * b))

squareModulus :: Num a => Complex a -> a
squareModulus (Complex (a, b)) = (a * a) + (b * b)

arg :: RealFloat a => Complex a -> a
arg (Complex (a, b)) = atan2 b a

toPolar :: RealFloat a => Complex a -> (a, a)
toPolar z = (modulus z, arg z)

fromPolar :: RealFloat a => (a, a) -> Complex a
fromPolar (r, t) = Complex (r * cos t, r * sin t)

instance Num a => Num (Complex a) where
    (+) :: Num a => Complex a -> Complex a -> Complex a
    (+) (Complex (a, b)) (Complex (c, d)) = toComplex (a + c, b + d)

    (*) :: Num a => Complex a -> Complex a -> Complex a
    (*) (Complex (a, b)) (Complex (c, d)) = toComplex ((a * c) - (b * d), (a * d) + (b * c))

    negate :: Num a => Complex a -> Complex a
    negate (Complex (x, y)) = toComplex (-x, -y)

    fromInteger :: Num a => Integer -> Complex a
    fromInteger x = toComplex (fromInteger x, 0)

    abs :: Num a => Complex a -> Complex a
    abs = error "Use modulus"

    signum :: Num a => Complex a -> Complex a
    signum = undefined

instance (Fractional a) => Fractional (Complex a) where
    (/) :: Fractional a => Complex a -> Complex a -> Complex a
    (/) (Complex (a, b)) (Complex (c, d)) = Complex (((a * c) + (b * d)) / ((c * c) * (d * d))
                                                    ,((b * c) - (a * d)) / ((c * c) * (d * d)))

    fromRational :: Fractional a => Rational -> Complex a
    fromRational a = Complex (fromRational a, 0)

instance RealFloat a => Floating (Complex a) where
    pi :: Fractional a => Complex a
    pi = Complex (pi, 0)

    exp :: Floating a => Complex a -> Complex a
    exp (Complex (x, y)) = Complex (exp x * cos y, exp x * sin y)

    log :: RealFloat a => Complex a -> Complex a
    log z = Complex (log $ modulus z, arg z)

    sin :: Floating a => Complex a -> Complex a
    sin (Complex (x, y)) = Complex (sin x * cosh y, cos x * sinh y)

    cos :: Floating a => Complex a -> Complex a
    cos (Complex (x, y)) = Complex (cos x * cosh y, -(sin x * sinh y))

    asin :: RealFloat a => Complex a -> Complex a
    asin z = (-i) * log (sqrt (1 - (z * z)) + (i * z))

    acos :: RealFloat a => Complex a -> Complex a
    acos z = (-i) * log (i * sqrt (1 - (z * z)) + z)

    atan :: RealFloat a => Complex a -> Complex a
    atan z = (i / (-2)) * log ((i - z) / (i + z))

    sinh :: Floating a => Complex a -> Complex a
    sinh z = (-i) * sin (i * z)

    cosh :: Floating a => Complex a -> Complex a
    cosh z = cos (i * z)

    tanh :: Floating a => Complex a -> Complex a
    tanh z = (-i) * tan (i * z)

    asinh :: RealFloat a => Complex a -> Complex a
    asinh z = log $ z + sqrt (z * z + 1)

    acosh :: RealFloat a => Complex a -> Complex a
    acosh z = log $ z + sqrt (z * z - 1)

    atanh :: RealFloat a => Complex a -> Complex a
    atanh z = 0.5 * log ((1 + z) / (1 - z))

instance (Show a, Num a, Ord a) => Show (Complex a) where
    show :: Show a => Complex a -> String
    show (Complex (x, y)) = if y >= 0
                            then show x ++ " + " ++ show y ++ "i"
                            else show x ++ " - " ++ tail (show y) ++ "i"

instance Functor Complex where
    fmap :: (a -> b) -> Complex a -> Complex b
    fmap f (Complex (x, y)) = Complex (f x, f y)

instance Applicative Complex where
    pure :: a -> Complex a -- kinda useless tbh
    pure x = Complex (x, x)

    (<*>) :: Complex (a -> b) -> Complex a -> Complex b
    (<*>) (Complex (f, g)) (Complex (x, y)) = Complex (f x, g y)

instance Monad Complex where
    (>>=) :: Complex a -> (a -> Complex b) -> Complex b
    (>>=) (Complex (x, y)) f = Complex (re $ f x, im $ f y)