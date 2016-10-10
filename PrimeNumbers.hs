--Nosagie Asaolu

--maps a list of integers to their squares  
squareAll :: (Num a) => [a] -> [a]
squareAll [] = []
squareAll (x:xs) = x * x:[] ++ squareAll xs

--returns the minimum value in a list of orderabe items, 
myMin :: (Ord a) => [a] -> a 
myMin (x:xs) = (foldr (\x y -> if x < y then x else y) x xs) 

--map implemented with foldr function, anonymous function
map' :: (Num a) => (a -> b) -> [a] -> [b]
map' op xs= foldr (\x y -> (op x):y) [] xs 

--performs fold function in lef-associative manner
myfoldl :: (Num a) => (a->b->a) -> a -> [b] -> a
myfoldl _ baseCase []= baseCase
myfoldl op baseCase (
	x:xs) = myfoldl op (baseCase `op` x) xs

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [x] = [x]
mergeSort [] = []
mergeSort (xs) =  merge (mergeSort $ takeHalf 0 xs) (mergeSort $ takeHalf 1 xs)

{-Alternative implementation using only merge helper method
  mergeSort (xs) =  merge (mergeSort (take ((length xs)`div` 2) xs)) (mergeSort (drop ((length xs) `div` 2) xs))  
-}

--MergeSort Helper Methods
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x:(merge xs (y:ys))
					| otherwise = y: (merge ys (x:xs))

-- 0 => first half os list, 1 => second half of list
takeHalf :: Int -> [a] -> [a]
takeHalf half (xs) | half == 0 = take ((length xs)`div`2) xs
				   | half == 1 = drop ((length xs)`div`2) xs


--Gotta Hand it to you Pete, this was fun
--I deliberately restricted Type Parameters to Integers, as I'm working with primes)

sieveSundaram :: Int -> [Int]
sieveSundaram a = filter (\x -> x < a) (mapGeneratedValues a)

{-Alternative one-line implementation, with minimal helper functions
sieveSundaram a = filter (\x -> x < a) (map (\x -> (x*2)+1)(checkList [1..a] (mergeSort (filter (\x -> x < a) (map operator' (cartProd [1..a] [1..a]))))))
-}

--HELPER METHODS for sieveSundaram

--Maps values to new list with 2n+2
mapGeneratedValues :: Int -> [Int]
mapGeneratedValues a = map (\x -> (x*2)+1) (checkList [1..a] (numbersToExclude a))

--Returns list of numbers which result from applying i+j+2ij function to cartesian product of list [1..a]
numbersToExclude :: Int -> [Int]
numbersToExclude a = filter (\x -> x < a) (mapOriginalValues a)

--Maps original values to new list with i+j+2ij
mapOriginalValues :: Int -> [Int]
mapOriginalValues a = map operator' (cartProd [1..a] [1..a])  

--Applies the  i+j+2ij function to all tuples in a list of tuples
operator' :: (Int,Int) -> Int
operator' (x,y) | (y >= x)  = x + y + (2*x*y)
				| otherwise = operator' (y,x)   

--Cartesian Product of Two Lists
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

--Filters out all items in list 1 that are also in list2 
checkList :: [Int] -> [Int] -> [Int]
checkList xs [] = xs
checkList (x:xs) (y:ys) = checkList (filter (\z -> z /= y) (x:xs)) (ys)








