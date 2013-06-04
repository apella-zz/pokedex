-- | Functions to get the data on pokemon from the web.
module DataGrab (getPokemonFromWeb) where
import Network.HTTP (getResponseBody, getRequest, simpleHTTP)
import PokeParser (parsePokemon, parseMissing)
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

-- | Get a given pokemon from the web if it exists
getPokemonFromWeb :: String -> IO (Maybe Pokemon)
getPokemonFromWeb name = do
  str <-  getPokemonDataString name
  return $ if parseMissing str
         then  Nothing
         else Just $ parsePokemon str
