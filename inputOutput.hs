main :: IO()
main = putStr "enter a number: "
    >> readLn
    >>= (\n -> putStr ("the next number is " ++ show (n+1) ))
