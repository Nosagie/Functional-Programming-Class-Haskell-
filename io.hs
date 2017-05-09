-- main :: IO ()
-- main = readLn >>= (\n -> putStrLn (show (n + 1)))


-- Signatures
-- main::IO()
-- readLn:: Read a -> IO a
-- putStrLn :: String -> IO()
-- (>>) :: IO a -> IO b
-- (>>=) :: IO a -> (a -> IO b) -> IO b

-- main :: IO()
-- main = inAndOut
--
-- inAndOut :: IO()
-- inAndOut = do putStr "gimme a number"
--               x <- readLn
--               putStrLn ("The next number is " ++ show (x + 1))

data Tree a = Node a [Tree a]
            | EmptyTree

instance Functor Tree where
      fmap _ Nothing _ = Nothing
      fmap f x (y:xs) = f x . fmap f y xs
