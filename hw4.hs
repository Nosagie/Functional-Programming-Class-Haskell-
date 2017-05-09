{-# LANGUAGE InstanceSigs #-}
type Name = String

data Employee = Employee { name :: Name
                         , phone :: String
                         }
    deriving Show

data MyMaybe a  = MyNothing
                | MyJust a

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust a) = MyJust (f a)

instance Applicative MyMaybe where
  pure :: a -> MyMaybe a
  pure a = MyJust a

  (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  MyNothing <*> _ = MyNothing
  (MyJust f) <*> a = fmap f a

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

aParser :: Parser Char
aParser = Parser p
    where p :: String -> Maybe (Char, String)
          p ('a':xs) = Just ('a', xs)
          p _        = Nothing
