module Perm(Permutation(Perm),clean,trans,inverse,
    findCycles,CyclePerm(MkCyclePerm),
    fromPermutation, toPermutation,cycleLength) where
import Data.List
import Data.Monoid

data Permutation a = Perm [(a,a)] deriving (Show,Eq)
newtype CyclePerm a = MkCyclePerm (Permutation a) deriving (Show)

fromPermutation :: Permutation a -> CyclePerm a
fromPermutation x = MkCyclePerm x

toPermutation :: CyclePerm a -> Permutation a
toPermutation (MkCyclePerm x) = x

conc :: Eq a => [(a, a)] -> [(a, a)] -> [(a, a)]
conc as [] = as
conc [] bs = bs
conc (a@(f,t):as) bs = a':conc as restBs
        where
             a' = conc' a $ find (\(x,y) -> t == x) bs
             restBs = filter (\(x,y) -> t /= x) bs
             conc' (f, t) (Just (x, y)) = (f, y) 
             conc' (f, t) Nothing = (f, t)

instance Eq a => Monoid (Permutation a) where
        mempty = Perm []
        (Perm as) `mappend` (Perm bs) = Perm (conc as bs)

clean :: Eq a => Permutation a -> Permutation a
clean (Perm as) = Perm $ filter (\(f,t) -> f /= t) as

trans :: Eq a => a -> Permutation a -> a
trans f (Perm as) = trans' f $ find (\(x,y) -> f == x) as
        where
                trans' f (Just (x,y)) = y
                trans' f Nothing = f

inverse :: Permutation a -> Permutation a
inverse (Perm xs) = Perm $ map (\(f,t) -> (t,f)) xs

takeUntilCycle :: Eq a => [a] -> [a]
takeUntilCycle [] = []
takeUntilCycle (x:xs) = x : takeWhile (\y -> x /= y) xs
      
findCycle :: Eq a => Permutation a -> [(a,a)]
findCycle s@(Perm (x:xs)) = takeUntilCycle $ iterate (\(f,t) -> (t,(trans t s))) x

findCycles :: Eq a => Permutation a -> [CyclePerm a]
findCycles (Perm []) = []
findCycles s@(Perm xs) = (fromPermutation (Perm firstCycle)) : findCycles (Perm rest)
  where firstCycle = findCycle s
        rest = filter (\(f,t) -> notElem f (froms firstCycle)) xs
        froms xs = map (\(f, t) -> f) xs

cycleLength :: CyclePerm a -> Int
cycleLength (MkCyclePerm (Perm xs)) = length xs

{-
  - let action = inverse turnR `mappend` inverse turnD
  - foldl1 lcm (map (\x -> cycleLength x) $ findCycles action)
  -}
