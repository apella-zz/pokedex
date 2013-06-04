module PokedexGeneration where
import Control.Monad (mapM)
import Data.Maybe (maybe, catMaybes) 
import qualified DataGrabLocal as DG (getPokemon)
import qualified DataGrab as DG (getGenerationList)
import Pokemon
import System.IO (writeFile)

{- You can use these functions to generate a pokedex.-}
{- Use DataGrab.downloadGeneration to download a whole generation in a-}
{- folder, then get the generation list in a variable at the-}
{- interpreter and try calling PokedexGeneration.gp on it. If it-}
{- doesn't throw any exceptions you are done and you can store the-}
{- pokedex using PokedexGeneration.storePokedex. Else, see at what-}
{- pokemon it failed and add the data manually in the downloaded file.-}
{- Repeat calling gp until all is well. I might add rules to the-}
{- downloader/parser on the specific pokemon that are exceptions but-}
{- don't count on it. -- Apella -}


-- | Create a pokedex from the data on the website. Use storePokedex
-- to save and and lower the connections to the wiki.
-- generatePokedex :: Int -> IO [Maybe Pokemon]
generatePokedex :: Int -> IO Pokedex
generatePokedex gen = 
  DG.getGenerationList gen >>=
  maybe (return []) (mapM DG.getPokemon) >>= return . catMaybes

-- | get the pokemon from the list created by getGenerationList
-- It's in the IO monad because getPokemon is in the IO monad
gp :: Maybe [String] -> IO Pokedex
gp lst = maybe (return []) (mapM DG.getPokemon) lst >>= return . catMaybes

-- | Store the given pokedex in the pokeCache file
storePokedex :: String -> Pokedex -> IO ()
storePokedex outputFile pokedex = writeFile outputFile $ show pokedex
