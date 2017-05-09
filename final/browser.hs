import Text.HTML.TagSoup
import Data.List
import Text.Regex.Posix
import Network.HTTP


data Browser = Browser { getUrl :: String, getHtml :: String, getLinks :: [String]} 

getLinksFromHtml :: String -> [String] 
getLinksFromHtml rawhtml = getAllTextMatches $ rawhtml =~ "(www.[a-zA-Z0-9]+.comc)|(www.[a-zA-Z0-9]+.com.[a-zA-Z]+)" :: [String]

-- Method to get URL from user
getResponse :: String -> IO[Char]
getResponse website = do 
					  rsp <- simpleHTTP (getRequest website)
					  getResponseBody rsp


goto :: String -> IO Browser
goto url = do 
	    html <- getResponse url 
	    let links = getLinksFromHtml html
	    return (Browser url html links)

