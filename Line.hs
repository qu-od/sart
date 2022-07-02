module Line where

import Control.Monad (guard)
import Data.Function (on)
import Painter (Point (MakePoint))

linesIntersection :: (Ord n, Fractional n) => Line n -> Line n -> Maybe (Point n)
linesIntersection ln0 ln1 = do
    let lns@(ln0', _) = ((,) `on` offsetAndAngle) ln0 ln1
    x <- uncurry linearIntersection lns
    let p = MakePoint x (linearY ln0' x)
    guard $ isInLineSegment ln0 p
    guard $ isInLineSegment ln1 p
    pure p

epsilon :: Fractional n => n
epsilon = 10e-7

floatEq :: (Ord a, Fractional a) => a -> a -> Bool
floatEq a b = (a - b) < epsilon

isInLineSegment :: (Fractional n, Ord n) => Line n -> Point n -> Bool
isInLineSegment (Between (MakePoint x0 y0) (MakePoint x1 y1)) (MakePoint x y) =
    between x0 x1 x && between y0 y1 y
isInLineSegment (OffsetAndAngle o a) (MakePoint x y) =
    linearY (o, a) x `floatEq` y
isInLineSegment ln@(FromPoint (MakePoint x0 y0) _ (up, right)) (MakePoint x y) =
    if right
        then x >= x0
        else
            x <= x0
                && if up
                    then y >= y0
                    else
                        y <= y0
                            && linearY (offsetAndAngle ln) x `floatEq` y

linearY :: Num n => (n, n) -> n -> n
linearY (a, b) x = (a + x) * b

offsetAndAngle :: Fractional n => Line n -> (n, n)
offsetAndAngle (OffsetAndAngle offset a) = (offset, a)
offsetAndAngle (FromPoint (MakePoint x0 y0) a _) =
    (a, (y0 / a) - x0)
offsetAndAngle (Between (MakePoint x0 y0) (MakePoint x1 y1)) =
    let b = (y0 - y1) / (x0 - x1)
        a = (b / y0) - x0
     in (a, b)

between :: Ord a => a -> a -> a -> Bool
between x0 x1 x = min x0 x1 <= x && max x0 x1 >= x

--
-- pointInAnyArea :: (Foldable t, Ord a, Applicative t) => t (Point a) -> Point a -> Bool
-- pointInAnyArea area pt = any (uncurry $ pointInRect pt) pairs
--     where
--     pairs = (,) <$> area <*> area
-- >>> pts
-- (True,True,True)

-- pts = (pointInAnyArea area inside, pointInAnyArea area outside, really)
--     where
--         outside = pt 1139 407
--         inside = pt 1292 503
--         pt = MakePoint
--         really = pointInRect inside (pt 1349 601) (pt 1227 333)
--         area = [pt 1113 545, pt 1349 601, pt 1172 345, pt 1227 333]

pointInRect (MakePoint x y) (MakePoint x0 y0) (MakePoint x3 y3) =
    between x0 x3 x && between y0 y3 y

data Line n
    = Between (Point n) (Point n)
    | OffsetAndAngle n n
    | FromPoint (Point n) n (Bool, Bool)

linearIntersection :: (Fractional n, Eq n) => (n, n) -> (n, n) -> Maybe n
linearIntersection (a0, b0) (a1, b1)
    | b1 == b0 = Nothing -- lines doesn't intersect
    | otherwise = Just $ (a0 * b0 - a1 * b1) / (b1 - b0)
