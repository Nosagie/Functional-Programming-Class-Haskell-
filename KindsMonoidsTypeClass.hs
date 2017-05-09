--Define Kinds
--define Monoid instances for Bool and (almost) function
--describe the function typeclass
--define instance of the function typeclass

data Thing = Widget  --"Thing" is a type
             | Whatsit
             | Gadget
             | Gizmo   --These words in red are value constructors- in patterns and left and right parts of function def

data Person = Person String Int -- Value constructor is "Person" that takes parameters  "String" and "Int"

data Tree a = Leaf a
             | Node (Tree a)(Tree a) --type variables like "a" need to appear on the left side too

--We can have a tree that takes a type (a) and returns a type (Tree a) Tree :: Type -> Type

--KINDS
-- :k Tree says tree is a type that takes a Type and gives a Type
