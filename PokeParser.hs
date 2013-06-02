module PokeParser where
import Control.Monad (liftM)
import Data.List (stripPrefix)
import Data.Maybe
import Pokemon -- contains the types
import System.IO (readFile)
import Text.Regex.Base
import Text.Regex.TDFA


-- | Parse the text and return the requested property (in String format)
readProp text prop =
  head hits
  where 
    (_, _, _, hits) = text =~ (prop ++ " * = *([^\n]+)") :: (String, String, String, [String])


-- | Parse the text and return a list of strings of the types of the
    -- pokemon.
readTypes :: String -> [Type]    
readTypes text =
  -- filter out the /{{type|<some-type>}} elements
  map read $ filter ((/= '/') . head)  hits 
  where -- the structure of one type def
    once =  "{{type\\|([^}]+)}}"
    (_, _, _, hits) = text =~ (once ++ "(/" ++ once ++ ")*" ):: (String, String, String, [String])


parseTypes :: String -> [Type]
parseTypes text = readTypes $ readProp text "type"


removeTags :: String -> String
removeTags = filter (flip notElem "[]")

-- | Parse the text and grab all the stats for the Stat type.
parseStats :: String -> Stat
parseStats text = 
  Stats hp atk def satk sdef spd total
  where
    [hp, atk, def, satk, sdef, spd, total] = map ((read :: String -> Int) . readProp text)
                                             ["hp", "atk", "def", "satk", "sdef", "spd", "total"]

parsePokedexId :: String -> Int
parsePokedexId text =
  read $ readProp text "ndex"

-- | Get the generation of the pokemon.
parseGeneration :: String -> Int
parseGeneration text =
  maybe 0 roman $ stripPrefix "Generation " $ removeTags $ readProp text "gen"
  -- not the most clean solution, but it'll do for now
  where roman "I" = 1
        roman "II" = 2
        roman "III" = 3
        roman "IV" = 4
        roman "V" = 5
        roman "VI" = 6


parsePokemon :: String -> Pokemon
parsePokemon text =
  Pokemon name types dex stats generation
  where name = readProp text "name"
        types = parseTypes text
        dex = parsePokedexId text
        stats = parseStats text
        generation = parseGeneration text
