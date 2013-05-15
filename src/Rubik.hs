{- |
Module      :  $Header$
Description :  Rubik's cube representation.
Copyright   :  (c) <Masato Sogame>
License     :  LLGPL

Maintainer  :  <poketo7878@gmail.com>
Stability   :  unstable
Portability :  portable
-}
module Rubik(Rubik, defaultRubik, trans,
    turnF,turnS,turnB,
    turnR, turnM, turnL,
    turnU, turnE, turnD) where
import qualified Perm as P
import Data.Monoid

data Color = White | Red | Orange 
            | Yellow | Green | Blue deriving (Show, Eq,Enum)

-- | Rubik represents rubik's cube state.
data Rubik = Rubik [(Int,Color)] deriving (Show, Eq)

-- | Create a normal state rubik's cube.
defaultRubik :: Rubik
defaultRubik = Rubik $ (zip [0..8] $ repeat White) ++ 
                        (zip [9..17] $ repeat Red) ++
                        (zip [18..26] $ repeat Orange) ++
                        (zip [27..35] $ repeat Yellow) ++
                        (zip [36..44] $ repeat Green) ++
                        (zip [45..53] $ repeat Blue)

{-
   [1]      [2]        [3]       [4]
  0 1 2 |  9 10 11 | 18 19 20 | 27 28 29 
  3 4 5 | 12 13 14 | 21 22 23 | 30 31 32 
  6 7 8 | 15 16 17 | 24 25 26 | 33 34 35 

    [5]        [6]
  36 37 38 | 45 46 47
  39 40 41 | 48 49 50
  42 43 44 | 51 52 53
-}

-- | Apply transform to rubik's cube.
trans :: Rubik -> P.Permutation Int -> Rubik
trans (Rubik xs) perm = Rubik $ map (\(f,c) -> ((P.trans f perm),c)) xs

--Util
indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..] xs

--Build turning
turnbelt :: ([Int],[Int],[Int],[Int]) -> [(Int, Int)]
turnbelt (a, b, c, d) = zip a b ++ zip b c ++ zip c d ++ zip d a

turnLeftSurf :: [Int] -> [(Int,Int)]
turnLeftSurf xs = map (\(i,v) -> (v, xs !! (2 - (div i 3) + mod i 3 * 3))) $ indexed xs

turnRightSurf :: [Int] -> [(Int, Int)]
turnRightSurf xs = map (\(i,v) -> (v, xs !! (6 + (div i 3) - mod i 3 * 3))) $ indexed xs

--Turn Symbols
turnF = P.Perm $  turnLeftSurf [0..8] ++
                turnbelt ([42,43,44], [9,12,15], [53,52,51], [35,32,29])

turnS = P.Perm $ turnbelt ([39,40,41],[19,22,25],[50,49,48],[34,31,28])

turnB = P.Perm $ turnLeftSurf [18..26] ++
               turnbelt ([38,37,36], [27,30,33], [45,46,47], [17,14,11])

turnR = P.Perm $ turnLeftSurf [9..17] ++
               turnbelt ([44,41,38], [18,21,24], [47, 50, 53], [8,5,2])

turnM = P.Perm $ turnbelt ([37,40,43],[1,4,7],[52,49,46],[25,22,19])

turnL = P.Perm $ turnLeftSurf [27..35] ++
               turnbelt ([36,39,41], [0,3,6], [51,48,45], [24,21,18])

turnU = P.Perm $ turnLeftSurf [36..44] ++
                turnbelt ([2,1,0],[29,28,27],[20,19,18],[11,10,9])

turnE = P.Perm $ turnbelt ([3,4,5],[12,13,14],[21,22,23],[30,31,32])

turnD = P.Perm $ turnRightSurf [45..53] ++
                turnbelt ([6,7,8],[15,16,17],[24,25,26],[33,34,35])

