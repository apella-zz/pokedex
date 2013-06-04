module Pokedex where
import Data.List  (find)
import Pokemon -- type definitions
import System.IO (readFile)

-- | the file that contains the pokedex list of pokemon already
-- downloaded. By keeping them on the hard drive we don't overuse
-- the wiki's bandwidth and speed up the process
pokedexFile :: String
pokedexFile = "./pokeCache.hs"

readPokedex :: IO Pokedex
readPokedex = readFile pokedexFile >>= return . (read :: String -> [Pokemon])

-- | Get a pokemon based on its name
getPokemon :: String -> Pokedex -> Maybe Pokemon
getPokemon pokemon pokedex = find (((==) pokemon) . name) pokedex

------
-- easy access functions:

-- | Get all the pokemon of a given type
getType :: Type -> Pokedex -> Pokedex
getType t  = 
  filter ((elem t) . types)

{- Commented out. The pokedex can be found in pokeCache.hs.
   We can't simply use the wiki because some pokemon don't have their
   data directly on the page but through a template.
   I might add specific code for those exceptions but I will also have
   to make changes to the Pokemon datatype as some pokemon's stats
   and/or class are different based on their form.
-}
-- | Get a pokemon. If it's not in the pokedex get it from the web.
-- Returns a tuple of the pokemon if found and the updated pokedex.
-- getPokemon :: String -> [Pokemon] -> IO (Maybe Pokemon, [Pokemon])
-- getPokemon pokeName pokedex = do
--   let p = find (((==) pokeName) . name) pokedex in
--     if isNothing p then
--       do pok <- DG.getPokemon pokeName
--          return (pok, insert (fromJust pok) pokedex)
--     else return (p, pokedex)
