{-Nosagie Asaolu *Functional Programming  *Pset 1-}

--calculates the number of items in a list of Integers
myLength :: [Integer] -> Integer
myLength [] = 0
myLength (firstElement:remainingElements) = 1 + myLength(remainingElements)

myLength' :: [a] -> Integer
myLength' [] = 0
myLength' (a:xs) = 1 + myLength'(xs)

--takes a list of Integers as its parameter and calculates their sum
mySum :: [Integer] -> Integer
mySum [] = 0
mySum (firstElement:remainingElements) = firstElement + mySum(remainingElements)

--takes a list of Integers as its parameter and calculates their product
myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (firstElement:remainingElements) = firstElement * myProduct(remainingElements)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (firstElement:remainingElements) = sumDigits(remainingElements) + digitSum(firstElement) 

chain :: (Int -> Int -> Int) -> Int -> [Int] -> Int
chain _ baseCase [] = baseCase
chain op baseCase (x:xs) = x `op` chain op baseCase xs

--Given an Integer, returns a list of its constituent digits
digitize :: Integer -> [Integer]
digitize 0 = []
digitize number = digitize (number `div` 10) ++ (number `mod` 10 : [])  

--Given an Integer, returns a list of its constituent digits, in reverse order.
digitizeRev :: Integer -> [Integer]
digitizeRev 0 = []
digitizeRev  number = (number `mod` 10 ) : digitizeRev(number `div` 10)

{-Given a list of Integers, returns a list of Integers i
 inn which every second value is doubled, starting with the second-to-last-}
doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond [] = []
doubleEverySecond (list) = listReverse(doubleEvenItems(listReverse(list))) 

--Returns True if and only if the given Integer is valid according to Luhn's algorithm
validateLuhn :: Integer -> Bool
validateLuhn number = sumDigits(doubleEverySecond(digitize(number))) `mod` 10 == 0

--Helper Functions
-- Finds sum of digits in Integer
digitSum :: Integer -> Integer
digitSum 0 = 0
digitSum number = digitSum(number `div` 10) + (number `mod` 10) 

--Reverses a list of Integers
listReverse :: [Integer] -> [Integer]
listReverse [] = []
listReverse (firstElement:remainingElements) =  listReverse remainingElements ++ (firstElement : []) 

listReverse' :: [a] -> [a]
listReverse' [] = []
listReverse' (firstElement:remainingElements) =  listReverse' remainingElements ++ (firstElement : []) 

-- Doubles second item in list of Integers, starting with the second element
doubleEvenItems :: [Integer] ->[Integer]
doubleEvenItems [] = []
doubleEvenItems [x] = [x]

doubleEvenItems (firstElement:secondElement:remainingElements) = firstElement : (secondElement*2) : [] ++ doubleEvenItems(remainingElements)



