module Symm(Symmetric(Symm),clean,trans,inverse,
    findCycles,CycleSymm(MkCycleSymm),
    fromSymmetric, toSymmetric,cycleLength) where
import Data.List
import Data.Monoid

data Symmetric a = Symm [(a,a)] deriving (Show,Eq)
newtype CycleSymm a = MkCycleSymm (Symmetric a) deriving (Show)

fromSymmetric :: Symmetric a -> CycleSymm a
fromSymmetric x = MkCycleSymm x

toSymmetric :: CycleSymm a -> Symmetric a
toSymmetric (MkCycleSymm x) = x

conc :: Eq a => [(a, a)] -> [(a, a)] -> [(a, a)]
conc as [] = as
conc [] bs = bs
conc (a@(f,t):as) bs = a':conc as restBs
        where
             a' = conc' a $ find (\(x,y) -> t == x) bs
             restBs = filter (\(x,y) -> t /= x) bs
             conc' (f, t) (Just (x, y)) = (f, y) 
             conc' (f, t) Nothing = (f, t)

instance Eq a => Monoid (Symmetric a) where
        mempty = Symm []
        (Symm as) `mappend` (Symm bs) = Symm (conc as bs)

clean :: Eq a => Symmetric a -> Symmetric a
clean (Symm as) = Symm $ filter (\(f,t) -> f /= t) as

trans :: Eq a => a -> Symmetric a -> a
trans f (Symm as) = trans' f $ find (\(x,y) -> f == x) as
        where
                trans' f (Just (x,y)) = y
                trans' f Nothing = f

inverse :: Symmetric a -> Symmetric a
inverse (Symm xs) = Symm $ map (\(f,t) -> (t,f)) xs

takeUntilCycle :: Eq a => [a] -> [a]
takeUntilCycle [] = []
takeUntilCycle (x:xs) = x : takeWhile (\y -> x /= y) xs
      
findCycle :: Eq a => Symmetric a -> [(a,a)]
findCycle s@(Symm (x:xs)) = takeUntilCycle $ iterate (\(f,t) -> (t,(trans t s))) x

findCycles :: Eq a => Symmetric a -> [CycleSymm a]
findCycles (Symm []) = []
findCycles s@(Symm xs) = (fromSymmetric (Symm firstCycle)) : findCycles (Symm rest)
  where firstCycle = findCycle s
        rest = filter (\(f,t) -> notElem f (froms firstCycle)) xs
        froms xs = map (\(f, t) -> f) xs

cycleLength :: CycleSymm a -> Int
cycleLength (MkCycleSymm (Symm xs)) = length xs

{-
  - let action = inverse turnR `mappend` inverse turnD
  - foldl1 lcm (map (\x -> cycleLength x) $ findCycles action)
  -}
