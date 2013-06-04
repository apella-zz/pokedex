module Pokedex where
import Control.Monad (liftM)
-- import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity
import Pokemon -- type definitions
import System.IO (readFile)
import DataGrab (getPokemonFromWeb)

-- | the file that contains the pokedex list of pokemon already
  -- downloaded. By keeping them on the hard drive we don't overuse
  -- the wiki's bandwidth and speed up the process
pokedexFile :: String
pokedexFile = "./pokeCache.hs"

-- pokedex :: IO [Pokemon]
-- pokedex :: Op ()
-- pokedex =  readFile pokedexFile >>=  return . (read :: String ->  [Pokemon])

-- myList :: StateT [Int] IO [Int]
-- myList = do
--   put $ (read :: String -> [Int]) . (lift readFile) "test.txt"
--   b <- get
--   return b
