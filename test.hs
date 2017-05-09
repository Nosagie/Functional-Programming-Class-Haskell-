{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--Part 2:

-- put v's here?


{-
thrown out code
- tag Leaf a = 1
--tag Branch a = tag (Tree a) + tag (Tree a)
-}
--2a also a = size
newtype Size = Size Int
    deriving (Show, Num, Enum, Real, Ord, Eq, Integral)

data Tree a v = Leaf a v
			| Branch a (Tree a v) (Tree a v)
			deriving (Show)

tag :: Tree a v -> a
tag (Leaf a v) = a
tag (Branch a l r) = a

sLeaf :: (Num a) => v -> Tree a v
sLeaf v = Leaf 1 v

--annotation = sum of annotations of children
--Every node is a branch

--finds size of a branch

sBranch :: Tree Size v -> Tree Size v -> Tree Size v
sBranch (Leaf a v) (Leaf b w) = Branch 2 (Leaf a v) (Leaf b w)
sBranch l r = Branch (tag l + tag r) l r

-- --used tempName instead of !!
-- (!!) :: Size -> Tree Size v -> Maybe v
-- (!!) _ (Leaf a v) = Just v
-- (!!) ind (Branch a l r) = if (ind >= (tag l))
-- 					then (!!) (ind - (tag l)) r
-- 					else (!!) ind l


--support search by index


--b
newtype Priority = Priority Int
    deriving (Show, Num, Enum, Real, Ord, Eq, Integral, Bounded)

--data Tree Priority v = Leaf Priority v
--			| Branch Priority v (Tree Priority v) (Tree Priority v)


pLeaf :: v -> Priority -> Tree Priority v
pLeaf a b = Leaf b a

pBranch :: Tree Priority v -> Tree Priority v -> Tree Priority v
pBranch (Leaf a v) (Leaf b w) = if (a>=b)
							then Branch b (Leaf a v) (Leaf b w)--(Leaf b w)
							else Branch a (Leaf a v) (Leaf b w)--(Leaf a v)

pBranch t1 t2 = if ((tag t1)>=(tag t2))
							then t2
							else t1
-- pBranch Tree a v Tree b w = if (a>=w)

findLowest :: Tree Priority v -> Maybe v
findLowest (Branch a left right) = if (tag left >= tag right)
											then findLowest right
											else findLowest left
findLowest (Leaf a v) = Just v


instance Monoid (Size) where
    mempty = Size 0
    Size x `mappend` Size y = Size (x + y)

instance Monoid (Priority) where
    mempty = maxBound
    Priority x `mappend` Priority y = if (x >= y) then Priority y else Priority x --min



search :: (Monoid a) => (a -> Bool) -> Tree a v -> Maybe v
-- search p (Leaf a v) '='' if p then Just v else Nothing
search p t = searchHelper t p mempty
--call searchhelper with ignored stuff
--false we go right
-- if at leaf and leaf is true return v

searchHelper :: (Monoid a) => Tree a v -> (a -> Bool) -> a -> Maybe v
searchHelper (Leaf a v) p n = Just v --if (p n) then Just v else Nothing
searchHelper (Branch a l r) p n = if (p (tag l)) then searchHelper l p n else searchHelper r p (a `mappend` n)
--keep track of ignored stuff

indexSearch :: Tree Size v -> Size -> Maybe v
indexSearch t n = if (((tag t)-1) < n) then Nothing else search (>n) t

prioritySearch :: Tree Priority v -> Maybe v
prioritySearch t = search (== tag t) t
