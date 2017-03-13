{-#LANGUAGE ScopedTypeVariables#-}

import Network.HTTP
import Data.List
import Data.Char
import Text.Regex.Posix
import Control.Monad

-- for html parsing
import Text.HTML.TagSoup
import Text.Regex.Base
import Text.Regex.Posix.String
import Text.Regex.Posix.Wrap

-- for arrows
import Text.XML.HXT.Core
import Text.HandsomeSoup


-- INFORMAL DOCUMENTATION 
-- Necessary libraries are above
-- The main methods are:
-- fetchLinks':takes a http url input (as a string) returns a list of all href values on the page. 

-- crawlWithLimit : given a list of http urls, a search term and a depth, this method will use recursively calls the crawler function
-- 		for the maximum depth specified, and returns a list of all (links) that contain the query term  

-- crawler : functions as described above but implemented usins regex and without the HXT library
-- crawler' : same as above but implemented using Arrows as described in the HXT library 

-- helper methods are commented for reference.



-- HXT
fetchLinks' :: String -> IO[String]
fetchLinks' url = do 
		 text <- openURL url 
		 let doc = readString [withParseHTML yes, withWarnings no] text
		 res <- runX . xshow $ doc
		 links <- runX $ doc >>> css "a" >>> getAttrValue "href"
		 let cleanedLinks = map (\x -> if isContained x "http" then x else url++x) links 
		 print cleanedLinks
		 return cleanedLinks
	


-- Helper function to return url
openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

-- Method to get URL from user
getResponse :: String -> IO[Char]
getResponse website = do 
					  rsp <- simpleHTTP (getRequest website)
					  source <- getResponseBody rsp
					  return source


--Method to check IO String for term
searchIO raw term = do 
				let result = fmap (\x -> isContained x term) 
				return result 


getTagText :: String -> String-> [Char]
getTagText rawhtml toGet= fromBody $ parseTags rawhtml
			where fromBody =  unwords.words.innerText. take 1.dropWhile (~/= ("<"++toGet++">")) 


getLinksFromHtml :: String -> [String] 
getLinksFromHtml rawhtml = getAllTextMatches $ rawhtml =~ "(www.[a-zA-Z0-9]+.com)" :: [String]

-- Alternate regex to account for urls of form .com.**
-- (www.[a-zA-Z0-9]+.com)|(www.[a-zA-Z0-9]+.com)|(www.[a-zA-Z0-9]+.com.).{2}"
-- (www.[a-zA-Z0-9]+.com)|(www.[a-zA-Z0-9]+.com)
--Broswer
data Browser = Browser { getUrl :: String, getHtml :: String, getLinks :: [String]} 


--Recursive Crawler Abstraction
crawlWithLimit :: [String] -> String -> Int -> IO String
crawlWithLimit _ _ 0 = return "Done Crawling"
crawlWithLimit links searchTerm depth = do 
		 newLinks <- crawler links searchTerm depth 
		 crawlWithLimit newLinks searchTerm (depth-1) 



-- Web Crawler with HXT
crawler' :: [String] -> String -> Int -> IO [[String]]
crawler' links searchTerm depth= do 
	pageContents <- crawlPageContent links
	let validLinks = checkValid pageContents searchTerm
	let contained = generateContained links validLinks
	putStrLn "The following links contain the elements "
	print contained
	allLinks <- mapM fetchLinks' links 
	let num = length allLinks
	putStrLn "Number of links are generated: "
	print num	
	-- print allLinks
	return allLinks 


-- Web Crawler (with regex)
crawler :: [String] -> String -> Int -> IO [String]
crawler links searchTerm depth= do 
	pageContents <- crawlPageContent links
	let validLinks = checkValid pageContents searchTerm
	let contained = generateContained links validLinks
	putStrLn "The following links contain the elements "
	print contained
	putStrLn "The following links are generated "	
	let allLinks = map ("http://"++) (nub $ concatMap getLinksFromHtml pageContents)
	print allLinks
	return allLinks 


-- generates links containing term deliberately have non exaustive patterns as the two input sizes will always be equal
generateContained :: [String] -> [Bool] -> [String]
generateContained [x] [y] = if y then [x] else []
generateContained (x:xs) (y:ys)= if y then [x] ++ generateContained xs ys else [] ++ generateContained xs ys 

-- checks for valid links with search term
checkValid :: [String] -> String -> [Bool]
checkValid [x] term = [isContained x term]
checkValid (x:xs) term = [isContained x term] ++ checkValid xs term

-- -- Monadic Operations
crawlPageContent :: [String] -> IO [String]
crawlPageContent links = do 
				results <- mapM getResponse links
				return results


--Search function returns boolean that signifies if page contains word
searchPage :: String-> String-> Bool
searchPage word pageContent = isContained pageContent word 

isContained :: String -> String -> Bool
isContained rawhtml term | isInfixOf term rawhtml = True
						 | otherwise = False


-- Gets user input and initial list of links currently buggy
-- initscrape :: IO String
initscrape = do 
	   putStrLn "Hello, please enter url in the form \"www.*****.com\" :"
	   url <- getLine
	   putStrLn "Please Enter a Search Term: "	   
	   searchTerm <- getLine
	   putStrLn "Please enter depth: "
	   depth :: Int  <- readLn

	   rawhtml <-getResponse ("http://"++url)

	   let links = map ("http://"++) (nub $ getLinksFromHtml rawhtml)

	   crawlWithLimit links searchTerm depth 	   

	   putStrLn "Program Done"



