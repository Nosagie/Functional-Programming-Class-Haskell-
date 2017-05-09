--declare datatype haskell, datatype and possible values must be capitalized
data Thing = Widget
            | Gizmo
            | Gadget
            | Whatsit  -- List all valid values of datatype - they are constructors which construct values of the datatype
     deriving Show -- allows values of the datatype to be printed



data ConsList = Null
              | Int : []

--Examples
x :: Thing
x = Gizmo

stuff :: [Thing]
stuff = [Widget,Gadget,Gizmo,Gizmo]

--Code below pattern matches against possible values of "Thing"
isInspector :: Thing -> Bool
isInspector Gadget = True
isInspector _      = False


data Person = Person String Int -- Person on the right is non ambigious constructor, Person on the left is a datatype,CONVENTION
		    deriving Show

pete :: Person
pete =  Person "Pete" 16

--Method Compares which two people are better traveled, @sign can only be used in patterns
betterTraveled :: Person -> Person -> Person
betterTraveled a@(Person _ aCount) b@(Person _ bCount) -- @ is used to refer to the values in parentheses, so we don't need the names, thus the _
	| aCount > bCount = a
	| otherwise       = b

isTraveled :: Person -> Bool
isTraveled p = (case p of -- If places traveled is 0 or 1 return False; Else return True, as the person is travelled
    Person _ 0 -> False
    Person _ 1 -> False
    Person _ _ -> True)

--OR alternate definition with syntactic sugar
isTraveled' :: Person ->Bool
isTraveled' p = not abc
    where abc = case p of 
        Person _ 0 -> False
        Person _ 1 -> False
        Person _ _ -> True 

--OR alternate definition with syntactic sugar
isTraveled'' :: Person ->Bool
isTraveled'' p = not abc
    where abc = case p of 
        Person _ (bar 0) -> False
        Person _ (bar 1) -> False
        Person _ _ -> True 
    bar x = x * 12

--Example of using Let Keyword
foo :: Int -> Int 
foo x = let y = 19 
            z = 22 
            in y * z * x

--Div function for doubles
data MaybeDouble = Good Double 
                 | Failure 

a :: MaybeDouble
a = Good 1.2

b :: MaybeDouble
b  = Failure

safeDiv :: Double -> Double -> MaybeDouble
safeDiv _ 0 = Failure
safeDiv x y = Good (x/y) 

--Use tyoe Variables to make functions declare any type
{-Interferes with default definintion
data Maybe a = Just a
                | Nothing
        deriving Show  -}

p :: Maybe Person
p = Nothing

