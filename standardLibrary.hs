--standard library called the prelude
--Use hOOGLE

import Data.List

--import custom module
--import Bar in other file
--import qualified Data.Text as T
--import Bar (head,asdf)

foo::[Int] -> Int
foo = foldl' (+) 0

--Create Module
module Bar where
	head :: [a] -> a
	head[] = undefined
	head(x:xs) = x


--Creating Types
type String = [Char]

--type Height = Int
--type defs can only take one argument
data Height = Height Int

isTall :: Height -> Bool
isTall (Height x) = x > 72

class Monoid m where
      mempty :: m
      mappend :: m -> m -> m

      mconcat :: [m] -> m
      mconcat = foldr mappend mempty

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

instance Monoid [a] where
	mempty = []
	mappend = (++) 

