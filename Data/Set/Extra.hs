module Data.Set.Extra (
    module Set,
    mapMonotonic,
    mapMonotonicInjective,
    mapStrictMonoIncreasing,
    ordRangeSet,
    fromDistinctMonotonicList,
    fromMonotonicList,
) where

import Data.Set hiding (mapMonotonic)
import qualified Data.Set as Set

mapStrictMonoIncreasing :: (a -> b) -> Set a -> Set b
mapStrictMonoIncreasing = Set.mapMonotonic

ordRangeSet :: (Ord a, Enum a) => a -> a -> Set a
ordRangeSet x1 x2
    | x1 < x2 = Set.fromDistinctAscList [x1 .. x2]
    | otherwise = Set.fromDistinctDescList [x2 .. x1]

-- | Makes a Set from ascending or descending distinct list
fromDistinctMonotonicList :: Ord a => [a] -> Set a
fromDistinctMonotonicList xs@(x0 : x1 : _)
    | x0 > x1 = Set.fromDistinctDescList xs
    | otherwise = Set.fromDistinctAscList xs
fromDistinctMonotonicList xs = Set.fromDistinctAscList xs

fromMonotonicList :: Ord a => [a] -> Set a
fromMonotonicList xs@(x0 : x1 : _)
    | x0 > x1 = Set.fromDescList xs
    | otherwise = Set.fromAscList xs
fromMonotonicList xs = Set.fromDistinctAscList xs

-- | Actual monotonic map that does not require function to be ascending
mapMonotonic :: Ord b => (a -> b) -> Set a -> Set b
mapMonotonic f = fromMonotonicList . fmap f . Set.toList

{- | Same as 'mapMonotonic', except that requires injectivity from mapping function.
 This allows to avoid deleting duplicate elements.
-}
mapMonotonicInjective :: Ord b => (a -> b) -> Set a -> Set b
mapMonotonicInjective f = fromDistinctMonotonicList . fmap f . Set.toList
