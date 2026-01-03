module EllipticCurve where
import Control.Comonad.Env
import NumberTheory (inverseMod, binaryExpr, isPower2, factors)

-- Elliptic curve mod p, pair of numbers (a, b) that represent the coefficients
data EllipticCurveP = ECP Integer Integer Integer
    deriving Eq

-- point on an elliptic curve
data Point = Point Integer Integer | O
    deriving Eq

type ECPoint = Env EllipticCurveP Point

getA :: EllipticCurveP -> Integer
getA (ECP a _ _) = a

getB :: EllipticCurveP -> Integer
getB (ECP _ b _) = b

getMod :: EllipticCurveP -> Integer
getMod (ECP _ _ p) = p


order :: Num a => ECPoint -> a -- probably can be done more effieciently
order ecp = case extract ecp of
    O         -> 1
    Point _ 0 -> 2
    _         -> order' 2 (2 *>> ecp) 
    where 
    order' acc ap = case extract ap of
        Point _ 0 -> acc * 2
        O         -> acc
        _         -> order' (acc + 1) (ecp <+> ap)


(<+>) :: ECPoint -> ECPoint -> ECPoint
(<+>) p1 p2 = if ask p1 /= ask p2 then undefined else case (extract p1, extract p2) of
    (a, O) -> env (ask p1) a
    (O, b) -> env (ask p2) b
    (Point x1 y1, Point x2 y2)
        | x1 == x2 && y1 == y2  -> env (ask p1) (Point (x3 slopeInst `mod` p) (y3 slopeInst `mod` p))
        | x1 == x2 && y1 == -y2 -> env (ask p1) O
        | otherwise             -> env (ask p1) (Point (x3 slopeDiff `mod` p) (y3 slopeDiff `mod` p))
        where
        slopeInst = (3 * x1 * x1 + getA (ask p1)) * ((2 * y1) `inverseMod` p)
        slopeDiff = (y2 - y1) * ((x2 - x1) `inverseMod` p)
        x3 m      = (m * m) - x1 - x2
        y3 m      = -(y1 + (m * (x3 m - x1)))
        p = getMod $ ask p1

(*>>) :: Integer -> ECPoint -> ECPoint
(*>>) n ecp
    | n == 0     = env (ask ecp) O
    | n == 2     = ecp <+> ecp
    | isPower2 n = ((n `div` 2) *>> ecp) <+> ((n `div` 2) *>> ecp)
    | otherwise  = foldr ((<+>) . (*>> ecp)) (env (ask ecp) O) (binaryExpr n)