-- | Functions to get the data on pokemon from the web.
module DataGrab (getPokemon, getGenerationList) where
import Network.HTTP (getResponseBody, getRequest, urlEncode, simpleHTTP)
import PokeParser (parsePokemon, parseMissing, parseGenerationCategory)
import Pokemon (Pokemon)
{-
to get the data:
format can be xml, json etc.
titles is the list of names (separated by | ) of the pages that we wish
http://pokemon.wikia.com/api.php?format=xml&action=query&prop=revisions&rvprop=content&titles=Pidgey

xml header:
<api><query><pages><page pageid="2391" ns="0" title="Pidgey"><revisions><rev xml:space="preserve">
and now the actual page content
-}
getPokemonDataString :: String -> IO String
getPokemonDataString pokemon =
  simpleHTTP (getRequest $ "http://pokemon.wikia.com/api.php?format=xml&action=query&prop=revisions&rvprop=content&titles=" ++ pokemon) >>= getResponseBody


downloadPokemon :: String -> String -> IO ()
downloadPokemon folder pokemon =
  getPokemonDataString pokemon >>= writeFile (folder ++ "/" ++ pokemon ++ ".xml") 
  
downloadGeneration :: String -> Int -> IO ()
downloadGeneration folder gen =
  getGenerationList gen >>=
  maybe (return ()) (mapM_ (downloadPokemon folder))

-- | Get a given pokemon from the web if it exists
getPokemon :: String -> IO (Maybe Pokemon)
getPokemon name = do
  str <-  getPokemonDataString name
  return $ if parseMissing str
         then  Nothing
         else Just $ parsePokemon str

----------
-- Generation retrieval

-- | Get the list of pokemon from the given generation.
getGenerationList :: Int -> IO (Maybe [String])
getGenerationList gen = do
  text <- getGenerationDataString gen
  return $ if parseMissing text
           then Nothing
           else Just $ parseGenerationCategory text

generationString i = maybe "O" id $ lookup i [(1, "I"), (2, "II"), (3, "III"),
                                              (4, "IV"), (5, "V"), (6, "VI")]

-- | Generate the string that allows us to get the requested generation.
getGenerationDataString :: Int -> IO String
getGenerationDataString gen =
  -- we must use urlEncode because Network.HTTP doesn't like the é. We
  -- can't do it for the whole string though because it changes the
  -- :// symbols and it doesn't like that either...
  let generationTitle = urlEncode $ "Category:Generation_" ++
                        (generationString gen) ++ "_Pokémon" in
  simpleHTTP (getRequest $  "http://pokemon.wikia.com/api.php?format=xml&action=query&prop=revisions&rvprop=content&titles=" ++ generationTitle) >>= getResponseBody
  
