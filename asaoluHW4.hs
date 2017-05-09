{-# LANGUAGE GeneralizedNewtypeDeriving #-}

fib :: Integer  -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs :: Integer -> [Integer]
fibs n = [fib n] ++ fibs (n+1)

fibs2 :: Integer -> Integer -> [Integer]
fibs2 x y = [x] ++ (fibs2 (y) (x+y))


fibs3 :: Integer -> Integer -> [Integer]
fibs3 x y = (zipWith (+) [x] [y])  ++ fibs3 (y) (x+y)

data MyEither a b= MyLeft a
                |MyRight b

--Answer for MyEither question 6
{-
  The error value is defined on the left because, when we define the Functor instance
  for MyEither, we need to make it fixed as the first value so we can pass the actual
  values to the fmap functions.
-}
instance Functor (MyEither a) where
      fmap f (MyLeft a) = MyLeft (a)
      fmap f (MyRight b) = MyRight (f b)


data Tree a v = Leaf a v
               | Branch a (Tree a v) (Tree a v)
            deriving Show

tag :: (Tree a v) -> a
tag (Leaf x _) = x
tag (Branch x _ _) = x

newtype Size = Size Int
  deriving (Show, Num, Enum, Real, Ord, Eq, Integral)

sLeaf :: Int -> (Tree (Size) Int)
sLeaf x  = Leaf (Size x) 1

sBranch :: (Tree (Size) Int ) -> (Tree (Size) Int) -> (Tree Size Int)
sBranch (Leaf x y) (Leaf d f) = Branch (x + d) (Leaf x y) (Leaf d f)
sBranch b@(Leaf x _) a@(Branch z _ _) = Branch (x + z) b a
sBranch a@(Branch z _ _) b@(Leaf x _) = Branch (x + z) a b
sBranch a@(Branch z _ _) b@(Branch w _ _) = Branch (z + w) b a

--Assuming the input is never negative
--if we don't make that assumption then we add this as 1st guard  | y < 0 = Nothing
(!*) :: (Tree (Size) q) -> Size -> Maybe q
(Leaf a v) !* y |(y == 0) = Just v
                | otherwise = Nothing
Branch _ c@(Leaf b _) d !* y | (y < 0) = Nothing
                             | (y < b) = c !* y
                             | otherwise = d !* (y-b)
Branch _ a@(Branch b _ _) c !* y | (y < 0) = Nothing
                                 | (y < b) = a !* y
                                 |otherwise = c !* (y-b)

newtype Priority = Priority Int
    deriving (Show, Num, Enum, Real, Ord, Eq, Integral, Bounded)

pLeaf :: a -> Priority -> Tree Priority a
pLeaf v b  =  Leaf b v

pBranch :: Tree Priority v -> Tree Priority v -> Tree Priority v
pBranch a@(Leaf x _) b@(Leaf y _) = Branch (min x y) a b
pBranch a@(Leaf x _) b@(Branch y _ _) = Branch (min x y) a b
pBranch a@(Branch x _ _) b@(Leaf y _) = Branch (min x y) a b
pBranch a@(Branch x _ _) b@(Branch y _ _) = Branch (min x y) a b

findLowest :: Tree Priority v -> Maybe (Tree Priority v)
findLowest (Leaf x y) = Just (Leaf x y)
findLowest (Branch z a@(Leaf x _) b) | z == x = Just a  | otherwise = findLowest b
findLowest (Branch z a@(Branch x _ _) b) | z == x = findLowest a  | otherwise = findLowest b

instance Monoid Size where
  mempty = Size 0
  mappend (Size x) (Size y) = Size (x+y)

instance Monoid Priority where
  mempty = Priority maxBound
  mappend (Priority x) (Priority y) | x < y = Priority x | otherwise = Priority y

leaf ::  p -> v -> Tree p v
leaf x y = Leaf x y

branch ::(Monoid a)=> Tree a b -> Tree a b -> Tree a b
branch a@(Leaf x _) b@(Leaf y _) = Branch (x `mappend` y) a b
branch a@(Leaf x _) b@(Branch y _ _) = Branch (x `mappend` y) a b
branch a@(Branch x _ _) b@(Leaf y _) = Branch (x `mappend` y) a b
branch a@(Branch x _ _) b@(Branch y _ _) = Branch (x `mappend` y) a b

--I tried my best
search :: (Monoid a)=> (a -> Bool) -> Tree a v -> Maybe v
search f t = searchHelper f t mempty

searchHelper :: (Monoid a) => (a -> Bool) -> Tree a v -> a -> Maybe v
searchHelper f (Leaf a v) _ | f a = Just v
                            | otherwise = Nothing
searchHelper f (Branch a l r) m | f (tag l) = searchHelper f l m
                                | otherwise = searchHelper f r (a `mappend` m)

indexSearch :: Tree Size v -> Size -> Maybe v
indexSearch t n | (tag t) <= n = Nothing
                | otherwise = search (> n) t

prioritySearch :: Tree Priority v -> Maybe v
prioritySearch t = search (== (tag t)) t
