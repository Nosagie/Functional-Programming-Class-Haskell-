foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ baseCase [] = baseCase
foldr' op baseCase (x:xs) = x `op` (foldr' op baseCase xs)

add :: Int -> Int 
add x  = x + 2

minus :: Int -> Int
minus x  = x - 2

(#) :: (b-> c) -> (a -> b) -> a -> c
(#) f g x = f (g x)

