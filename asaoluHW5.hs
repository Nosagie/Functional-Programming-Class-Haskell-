--Nosagie Asaolu
--Functional Programming
--HW 5

{-# LANGUAGE InstanceSigs #-}
import Data.Char

data MyMaybe a = MyJust a
                | MyNothing

instance Functor MyMaybe where
          fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
          fmap _ (MyNothing) = MyNothing
          fmap f (MyJust a) = MyJust (f a)

instance Applicative MyMaybe where
        pure :: a -> MyMaybe a
        pure  = MyJust
        (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
        (<*>) MyNothing _ = MyNothing
        (<*>) (MyJust f) x = fmap f x

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f pa = Parser pb where
       pb s = case (runParser pa s) of
        Nothing -> Nothing
        Just(x,s') -> Just (f x,s')

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \s->Just(x,s)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) p1 p2 = Parser p
        where p s =  case runParser p1 s of
                    Nothing -> Nothing
                    Just (f,s') -> case runParser p2 s' of
                                  Nothing -> Nothing
                                  Just(v,s') -> Just(f v,s')

aParser :: Parser Char
aParser = Parser p
    where p :: String -> Maybe (Char, String)
          p ('a':xs) = Just ('a', xs)
          p _        = Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy q = Parser p
    where p :: String -> Maybe(Char, String)
          p (x:xs) | (q x) = Just(x,xs) | otherwise = Nothing
          p _ = Nothing

char :: Char -> Parser Char
char c = Parser p where
    p :: String -> Maybe(Char,String)
    p (x:xs) | (c == x) = Just(c,xs) | otherwise = Nothing
    p _     = Nothing

abParser :: Parser (Char,Char)
abParser = pure f <*> char('a') <*> char('b')
    where f :: Char -> Char -> (Char,Char)
          f x y = (x,y)

charPair :: Char -> Char -> Parser (Char,Char)
charPair x y = pairHelper <$> pure x <*> pure y

pairHelper :: a -> a -> (a,a)
pairHelper x y = (x,y)

abParser_  :: Parser ()
abParser_ = pure f <*> char('a') <*> char('b')
    where f :: Char -> Char -> ()
          f x y = ()

intPair ::Parser (Integer,Integer)
intPair = f <$> posInt <*> (char ' ') <*> posInt
        where f :: Integer -> Char -> Integer -> (Integer,Integer)
              f x ' ' y = (x,y)

posInt :: Parser Integer
posInt = Parser f
    where f :: String -> Maybe (Integer, String)
          f [] = Nothing
          f xs | null ns = Nothing
               | otherwise = Just (read ns, rest)
            where (ns, rest) = span isDigit xs

anyCharBut :: Char -> Parser Char
anyCharBut c = satisfy(/= c)

orElse ::(Eq b) => Parser b -> Parser b -> Parser b
orElse (Parser pa) (Parser pb) = Parser pc
      where pc s | pa s == Nothing = pb s
                 | otherwise = pa s

many :: Parser a -> Parser [a]
many (Parser pa) = Parser pb
      where pb s = case pa s of
                 Nothing -> Just([],s)
                 Just (x,xs) -> runParser (pure f <*> Parser pa <*> Parser pb) s
                      where f::a-> [a] -> [a]
                            f x y = [x] ++ y

many1 :: Parser a -> Parser [a]
many1 pa = Parser pb
     where pb s = case runParser (many pa) s of
                Just ([],_) -> Nothing
                m -> m

sepBy :: Parser [a] -> Parser Char -> Parser[[a]]
sepBy (Parser pa) (Parser pb) = (Parser pc)
  where pc s =  case pa s of
            Just ([],xs) -> case pb s of
                  Nothing -> Just([],s)
                  Just (y,ys) -> runParser((Parser pc)) ys
                        where f x y = [x] ++ y
            Just (_,_) -> runParser (pure f <*> (Parser pa) <*> (Parser pc)) s
                  where f x y = [x] ++ y

lineParserHelp :: Parser[String]
lineParserHelp = sepBy (many (anyCharBut '\n'))(char '\n')

itemParserHelp :: Parser[String]
itemParserHelp = sepBy (many (anyCharBut ','))(char ',')

csvParser  :: Parser[[String]]
csvParser = Parser pa
  where pa s = case runParser lineParserHelp s of
              Just (xs,"")->Just ((f xs),"")
                    where f x = map support x
                                where support q = case runParser itemParserHelp q of
                                        Just (x,_) -> x
