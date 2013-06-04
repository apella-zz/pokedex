module DataGrabLocal where
import System.IO
import System.FilePath
import Pokemon
import PokeParser

-- The same interface as in DataGrab is provided so that the only
  -- thing we need to change is the import clause when switching.

-- default values
folderName = "xml"
fileExt = "xml"

getPokemonPath name =
  folderName </> name <.> fileExt

getPokemon :: String -> IO (Maybe Pokemon)
getPokemon pokemon = do
  str <- readFile (getPokemonPath pokemon)
  return $ if parseMissing str
           then Nothing
           else Just $ parsePokemon str

-- | Get the list of pokemon from the given generation. (not implemented
getGenerationList :: Int -> IO (Maybe [String])
getGenerationList gen = undefined
