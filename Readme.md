# Rubik's Cube in Haskell

Manipulating Rubik's cube operation in Haskell.

## Module Perm

### data Permutation a

Representing Permutation in Haskell

#### Constructor 

> Perm [(1,3), (2, 4)]

#### Operation

- clean :: Eq a => Permutation a -> Permutation a

remove identify substitution from Permutation

- inverse :: Permutation a -> Permutation

make inverse permutation.

- trans :: Eq a => a -> Permutation a -> a

translate variable by permutation.

#### Instances

- Monoid Permutation

### newtype CyclePerm a

#### Constructor

> MkCyclePerm (Permutation a)

  - fromPermutation :: Permutation a -> CyclePerm a
  - toPermutation :: CyclePerm a -> Permutation a

#### Operation

  - findCycles :: Eq a => Permutation a -> [CyclePerm a]

  convert a permutation to group of cyclic permutations.

  - cycleLength :: CyclePerm a -> Int

  cycle length of cyclic permutation.

## Module Rubik

### data Rubik

#### Constructor

  constructor not exported use defaultRubik instead.

#### defaultRubik

  > defaultRubik :: Rubik

  generate standard state rubik's cube.

#### Operation

  - trans :: Rubik -> Permutation Int -> Rubik

  apply transform to rubik's cube

  - turnF :: Permutation Int
  - turnS :: Permutation Int
  - turnB :: Permutation Int
  - turnR :: Permutation Int
  - turnM :: Permutation Int
  - turnL :: Permutation Int
  - turnU :: Permutation Int
  - turnE :: Permutation Int
  - turnD :: Permutation Int

  You can create other turning operation with inverse, mappend.

## And More..

Manipulating turning operation cycle with haskell.

> let action = P.inverse R.turnR `mappend` P.inverse R.turnD
> -- Calc cycle length of action.
> foldl1 lcm (map (\x -> P.cycleLength x) $ P.findCycles action)
> 105
