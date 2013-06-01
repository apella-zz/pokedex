-- | Functions to get the json data on pokemon etc.
module DataGrab where
import Control.Monad (ap, liftM)
import Data.Maybe (fromJust)
import System.IO (readFile)
import Text.XML.Light
import Text.XML.Light.Input (parseXML)
{-
to get the data:
format can be xml, json etc.
titles is the list of names (separated by | ) of the pages that we wish
http://pokemon.wikia.com/api.php?format=xml&action=query&prop=revisions&rvprop=content&titles=Pidgey

xml header:
<api><query><pages><page pageid="2391" ns="0" title="Pidgey"><revisions><rev xml:space="preserve">
and now the actual page content
-}
getContent :: Content -> [Content]
getContent (Elem x) = elContent x

getText :: Content -> String
getText (Text x) = cdData x

firstContent :: Element -> Content
firstContent x = head $ elContent x

apiLevel :: IO Element
apiLevel = readFile "pidgey.xml" >>= return . fromJust . parseXMLDoc

queryLevel :: Element -> Content
queryLevel = head . elContent

pages :: Element -> [Content]
pages = getContent . queryLevel

firstPage :: Element -> Content
firstPage = head . getContent . head . pages

revisions :: Element -> [Content]
revisions = getContent . firstPage

firstRevision = head . getContent . head . revisions

coda = getText . head . getContent . firstRevision

pidgey = liftM coda apiLevel
-- firstPage :: IO Content
-- firstPage = return head `ap` pages

-- revisions page = head $ getContent page 

-- --            <rev>        <revisions> <page>
-- content api = getContent $ revisions $ 

-- element has multiple content
-- content has one element

-- | takes the name of a pokemon and returns a string with the json-data on it.
-- getPokemonString :: String -> IO String
-- getPokemonString name = 
  
-- | Takes an element `e` and a list and will return the elements after the first occurance of `e`         
dropUntil :: Eq a => a -> [a] -> [a]
dropUntil _ [] = []
dropUntil e (x:ls) = if e == x then ls else dropUntil e ls