
data Tree d = Leaf d
			| Node d(Tree d) (Tree d)
			| EmptyTree

instance (Show c) => Show (Tree c) where
	show (EmptyTree) = "EmptyTree"
	show (Leaf x) = " Leaf " ++ show(x)
	show (Node x (y) (z)) = " Node " ++ show(x)  ++ show(y) ++ show(z) ++ " "

treeLookup :: (Ord a) => Tree a -> a -> Bool
treeLookup (Leaf z) y = if (z == y) then True else False
treeLookup (Node x (Leaf y) (Leaf z)) v | (x == v) || (y == v) || (z == v) = True
										| otherwise = False
treeLookup (Node x y z) f = (x == f) || treeLookup y f || treeLookup z f

treeInsert :: Tree a -> a -> Tree a
treeInsert (Node a b EmptyTree) y = Node a (b) (Leaf y)
treeInsert EmptyTree y = Leaf y
treeInsert (Leaf x) y = Node x (Leaf y) (EmptyTree)
treeInsert (Node a b c) y = Node a (Node y (b) (c)) EmptyTree

treeDelete ::(Eq a,Ord a)=> Tree a -> a -> Tree a
treeDelete x y | (treeLookup x y == False) = x
			   | otherwise = deleteVal x y

deleteVal ::(Eq a)=> Tree a -> a -> Tree a
deleteVal EmptyTree _ = EmptyTree
deleteVal (Leaf x) a | x == a = EmptyTree
					 | otherwise = Leaf x
deleteVal (Node a b c) d | (a == d) = treeBuild (deleteVal (b) d) (deleteVal (c) d)
						   | otherwise = treeInsert (treeBuild (deleteVal (b) d) (deleteVal (c) d)) a

treeBuild :: Tree a -> Tree a -> Tree a
treeBuild EmptyTree EmptyTree = EmptyTree
treeBuild EmptyTree b = b
treeBuild b EmptyTree = b
treeBuild (Leaf x) b@(Node _ _ _) = treeInsert b x
treeBuild b@(Node _ _ _ ) (Leaf x) = treeInsert b x
treeBuild (Leaf x) y@(Leaf _) = treeInsert y x
treeBuild a@(Node x y z) b@(Node e r t) = Node x y (treeBuild z b)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ EmptyTree = EmptyTree
treeMap op (Leaf x) = Leaf (op x)
treeMap op (Node x y z) = Node (op x) (treeMap op y) (treeMap op z)

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold _ baseCase EmptyTree = baseCase
treeFold op baseCase (Leaf x) = x `op` baseCase
treeFold op baseCase (Node x y z) = x `op` (treeFold op baseCase (treeBuild y z))

--Why isn't telling us to implement tree filter?
--Because it messes up the structure of the underlying tree

class Aesthetic a where
	isPretty :: a -> Bool

instance Aesthetic Int where
	isPretty z = (foldr (\x y -> x + y) 0 (filter (\y -> z `mod` y == 0) [1..(z`div`2)])) == z

instance Aesthetic Integer where
    isPretty z =  (foldr (\x y -> x + y) 0 (filter (\y -> z `mod` y == 0) [1..(z`div`2)])) == z

instance Aesthetic (Tree a) where
	isPretty EmptyTree = True
	isPretty (Leaf a) =  True
	isPretty (Node x y z) = (isPretty y) && (isPretty z) && (depthDiff (treeDepth y) (treeDepth z))

max' :: Integer -> Integer -> Integer
max' x y | x > y = x
		 | otherwise = y

depthDiff ::Integer -> Integer-> Bool
depthDiff x y | x > y = (x - y) <= 1
			  | otherwise = (y - x) <= 1

treeDepth :: Tree a -> Integer
treeDepth (Leaf a) = 1
treeDepth a@(Node x y z) = max' (1 + subTreeDepth 0 y) (1 + subTreeDepth 1 z)

subTreeDepth :: Integer -> Tree a -> Integer
subTreeDepth _ EmptyTree = 0
subTreeDepth half (Leaf a) = 1
subTreeDepth half (Node x y z) = if half == 0  then 1 + subTreeDepth half y else  1 + subTreeDepth half z
