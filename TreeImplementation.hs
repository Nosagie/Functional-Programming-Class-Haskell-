{-Nosagie Asaolu
  Problem Set 3

  I deliberately use parenthesis as they are clearer 
-}

--"EmptyTree" Constructor represents an Tree with no value 
data Tree a = Leaf a
			| Node a(Tree a) (Tree a)
			| EmptyTree 

--I deliberately use parenthesis as it looks clearer to me
instance (Show a) => Show (Tree a) where
	show (EmptyTree) = " Empty Tree "
	show (Leaf x) = " (Leaf " ++ show(x) ++ ") "
	show (Node x (y) (z)) = " (Node " ++ show(x)  ++ show(y) ++ show(z) ++ ") "

--Ord typeclass is used to restrict parameters to comparable values 
treeLookup :: (Ord a) => Tree a -> a -> Bool
treeLookup (Leaf x) y| (x == y) = True
					 | otherwise = False	
treeLookup (Node x (Leaf y) (Leaf z)) b | (x == b) || (y == b) || (z == b) = True
										| otherwise = False
treeLookup (Node x b@(_) c@(_)) f = (x == f) || treeLookup b f || treeLookup c f									

--Inserts value into tree, value must be of same type as other tree elements
treeInsert :: Tree a -> a -> Tree a
treeInsert EmptyTree y = Leaf y
treeInsert (Node a b EmptyTree) y = Node a b $ Leaf y
treeInsert (Leaf x) y = Node x (Leaf y) EmptyTree
treeInsert (Node a b c) y = Node a (Node y b c) EmptyTree

--Eq needed to lookup values in Tree 
treeDelete ::(Eq a,Ord a)=> Tree a -> a -> Tree a
--First check if tree contains value, if not, return original tree, else use delete helper function
treeDelete x y | treeLookup x y == False = x
			   | otherwise = deleteVal x y   

--Helper functions to delete val and rebuild tree
deleteVal ::(Eq a)=> Tree a -> a -> Tree a
deleteVal EmptyTree _ = EmptyTree
deleteVal (Leaf x) a | x == a = EmptyTree 
					 | otherwise = Leaf x
deleteVal (Node a b c) d | (a == d) = rebuildTree (deleteVal (b) d) (deleteVal (c) d)
						   | otherwise = treeInsert (rebuildTree (deleteVal (b) d) (deleteVal (c) d)) a


--Builds new tree from 2 sub trees
rebuildTree :: Tree a -> Tree a -> Tree a
rebuildTree EmptyTree EmptyTree = EmptyTree --May not be necessary, but doesn't harm anything by being present
rebuildTree EmptyTree b = b
rebuildTree b EmptyTree = b
rebuildTree (Leaf x) b@(Node _ _ _) = treeInsert b x
rebuildTree b@(Node _ _ _ ) (Leaf x) = treeInsert b x
rebuildTree (Leaf x) y@(Leaf _) = treeInsert y x
rebuildTree (Node x y z) b@(Node _ _ _) = Node x y (rebuildTree z b) 

--performs Map function on Tree
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ EmptyTree = EmptyTree
treeMap op (Leaf x) = Leaf (op x)
treeMap op (Node x y z) = Node (op x) (treeMap op y) (treeMap op z) 

--performs Fold function on Tree  (right associative)
treeFold :: (a -> b -> b) -> b -> Tree a -> b 
treeFold _ baseCase EmptyTree = baseCase
treeFold op baseCase (Leaf x) = x `op` baseCase
treeFold op baseCase (Node x y z) = x `op` (treeFold op baseCase (rebuildTree y z))

--Why aren't you asking us to implement Tree Filter? I really don't know so I implemented it
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter _ EmptyTree = EmptyTree
treeFilter op (Leaf a) | op a == True = Leaf a
					   | otherwise = EmptyTree
treeFilter op (Node x y z) | op x == True = rebuildTree (rebuildTree (Leaf x) (treeFilter op y)) (treeFilter op z)
						   | otherwise = rebuildTree (treeFilter op y) (treeFilter op z)


class Aesthetic a where
	isPretty :: a -> Bool

--Range stops at x/2 as for any number x, its factors are less than x/2
instance Aesthetic Int where
	isPretty z = (foldr (\x y -> x + y) 0 (filter (\y -> z `mod` y == 0) [1..(z`div`2)])) == z 

instance Aesthetic Integer where
    isPretty z =  (foldr (\x y -> x + y) 0 (filter (\y -> z `mod` y == 0) [1..(z`div`2)])) == z 

--EmptyTrees and Leafs are "pretty" as their depth is 0 and 1 respectively, same with their nonexistent subtrees
instance Aesthetic (Tree a) where
	isPretty EmptyTree = True 
	isPretty (Leaf _) =  True
	isPretty a@(Node x y z) = (checkDiff (depth y) (depth z)) && (isPretty y) && (isPretty z) --None Recursive operation first #LazyEval

--HELPER FUNCTIONS FOR ISPRETTY
--returns max of two Integers, didn't use max in standard library as I don't remember covering it in class
max' :: Integer -> Integer -> Integer
max' x y | x > y = x
		 | otherwise = y
--checks if difference between 2 numbers is <= 1
checkDiff ::Integer -> Integer-> Bool
checkDiff x y | x > y = (x - y) <= 1
			  | otherwise = (y - x) <= 1
--returns depth of tree by getting max'(leftSubTree,RightSubTree) 
depth :: Tree a -> Integer
depth (Leaf _) = 1
depth (Node _ y z) = max' (1 + getDepth 0 y) (1 + getDepth 1 z) 
--return Depth of leftSubTree(when half is 0) or rightSubTree(when half is 1)
getDepth :: Integer -> Tree a -> Integer 
getDepth _ EmptyTree = 0
getDepth half (Leaf a) = 1
getDepth half (Node x y z) | half == 0  = 1 + getDepth half y 
						   | otherwise =  1 + getDepth half z


{- 
SOME FUN EXPERIMENTS

--Converts tree to List
toArrayT :: Tree a -> [a]
toArrayT EmptyTree = []
toArrayT (Leaf x) = [x] 
toArrayT a@(Node d e f) = toArrayT(e) ++ toArrayT(f) ++ [d] 

--Builds Tree from List
buildT :: (Eq a) => [Tree a] -> Tree a
buildT [] = EmptyTree
buildT [x] = Leaf x
buildT (x:y:xs) = treeInsert' (treeInsert' (Leaf x) (Leaf y)) (buildT xs) 

--Alternate insert method with unnecessary base cases..I got carried away....
treeInsert' :: Tree a -> Tree a -> Tree a
treeInsert' EmptyTree EmptyTree = EmptyTree
treeInsert' EmptyTree b@(Leaf _) = b
treeInsert' b@(Leaf _) EmptyTree  = b
treeInsert' EmptyTree x@(Node a b c) = x
treeInsert' x@(Node a b c) EmptyTree= x
treeInsert' a@(Leaf _)  (Leaf x) = treeInsert a x
treeInsert' x@(Node a b c) (Leaf y) = Node a (Node y (b) (c)) EmptyTree
treeInsert' (Leaf y) x@(Node a b c) = Node a (Node y (b) (c)) EmptyTree
treeInsert' y@(Node _ _ _) x@(Node a b c) =  EmptyTree
-}

--PS - It took me ~6-7 hours but I started yesterday as I've been sick all week - nevertheless, I really enjoyed it!



