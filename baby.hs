doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs x y z= doubleMe x + doubleMe y + doubleMe z

doubleSmallNumber x = if x > 100 then x else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 2

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

zips [] _ = []
zips _ [] = []
zips (x:xs) (y:ys) = (x,y) : zips xs ys

enumerate :: [[Char]] -> [(Integer,[Char])]
enumerate (x) = zips [1..] x  


chain :: (Int -> Int -> Int) -> Int -> [Int] -> Int 
chain _ baseCase [] = baseCase
chain op baseCase (x:xs) = x `op` (chain op baseCase xs) 

cups :: Integer -> [Integer]
cups x =  [x`div`16, (x`mod`16)`div`4,((x`mod`16)`mod`4)`div`2,(((x`mod`16)`mod`2)`mod`2)`div`1]

blow :: Int -> Int -> Int -> Int 
blow x y z= x * y * z 


qsort :: [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (<x) xs) ++ [x] ++ qsort (filter (>x) xs)



