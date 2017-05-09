--Recursive Datatype

data Tree a = Leaf a
			| Node (Tree a) (Tree a)
	deriving Show 

--Defining a Tree
t:: Tree Int
t = Node (Node(Leaf 1)(Leaf 2))(Node (Leaf 3) (Leaf 4))

--Application is left associative and definition is right associative


{-
	 We've seen type signatures that look like this: f::a -> a-> a 
	 (Polymorphism - f is polymorphic - parametric polymorhpism 
	 as the parameters are polymorphic )
-}

{-
	Error as there is a mismatch in type signature and implementation
	f :: a -> a -> a -> a
	f x y = x && y 
-}

{-
	Const function
	f:: a -> b -> a
	f x _ = x
-}

--Type Classes
--g :: (Ord a) => a -> a -> a
--g' :: (Ord a,Ord b) => a -> b -> a
--h :: (Ord a, Num a) => a -> a -> a

{-
EQ class sonflicts with standard library
class Eq a where
	(==) :: a -> a -> Bool
	(/=) :: a -> a -> Bool
	x /= y = not (x == y)

Cononical Implementation
class Eq a where
	(==),(=/) :: a -> a -> a -> Bool
	x == y = not (x /= y)
	x /= y = not (x == y)

data Foo = F Int | G char -- Integer or Character

instance Eq Foo where 
	(F i1) == (F i2) = i1 == i2
	(G i2) == (G c2) = c1 == c2
	_ == _ = False
-}






























