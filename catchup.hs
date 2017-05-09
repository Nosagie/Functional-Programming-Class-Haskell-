data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float
            | Rectangle Point Point
            deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r * r
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person {firstname :: String,
                      lastname :: String,
                      age :: Int,
                      height :: Float,
                      phonenumber :: String,
                      flavor :: String
                  }deriving (Show,Eq,Read)

data Car = Car { company::String,
                 model :: String,
                  year :: String
               }deriving Show

-- main = do line <- getLine
--           let line' = reverse line
--           putStrLn $ "You said " ++ line' ++ " backwards!"
--           putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"
