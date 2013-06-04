module PokeParser (parsePokemon, parseMissing, parseGenerationCategory) where
import Control.Monad (liftM)
import Data.Char (toLower, toTitle, isAlpha)
import Data.List (stripPrefix)
import Data.Maybe
import Pokemon -- contains the types
import System.IO (readFile)
import Text.Regex.Base
import Text.Regex.TDFA


-- | Parse the text and return the requested property (in String
  -- format)
readProp :: String -> String -> String
readProp text prop =
  head hits
  where 
    (_, _, _, hits) = text =~ (propTrans ++ " *= *([^\n]+)") :: (String, String, String, [String])
    -- The first letter could be capitalized or not. In both cases we
    -- need to read it correctly.
    propTrans = "[" ++ [small] ++ "|" ++ [big] ++ "]" ++ tail prop
    firstLetter = head prop
    small = toLower firstLetter
    big = toTitle firstLetter

-- | Parse the text and return a list of strings of the types of the
    -- pokemon.
readTypes :: String -> [Type]    
readTypes text =
  -- filter out the /{{type|<some-type>}} elements
  map read $ filter ((/= '/') . head) $ filter ((/= ""))  hits
  where -- the structure of one type def
    once =  "{{[t|T]ype\\|([^}]+)}}"
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

-- | the index in the pokedex of the pokemon
parsePokedexId :: String -> Int
parsePokedexId text =
  read $ readProp text "ndex"

-- | Get the generation of the pokemon.
parseGeneration :: String -> Int
parseGeneration text =
  maybe 0 roman $ stripPrefix "Generation " $ removeTags $ readProp text "gen"
  -- Not the most clean solution, but it'll do for now
  -- There is a package called roman-numerals on hackage
  -- (http://hackage.haskell.org/package/roman-numerals)
  -- but it seems like overkill to demand yet more packages.
  where roman "I" = 1
        roman "II" = 2
        roman "III" = 3
        roman "IV" = 4
        roman "V" = 5
        roman "VI" = 6

-- | parse the markup language and return a pokemon.
parsePokemon :: String -> Pokemon
parsePokemon text = 
  Pokemon name types dex stats generation
  where name = filter isAlpha $ readProp text "name"
        types = parseTypes text
        dex = parsePokedexId text
        stats = parseStats text
        generation = parseGeneration text

-- | Look if the string contains the indication that the requested
-- pokemon was not found.
parseMissing :: String -> Bool
parseMissing text =
  text =~ "<page ns=\"0\""

-- | Returns all the names of pokemon from a generation page
-- the parser can return a list of lists containing the matched text
-- and its submatches, which is what we use here.
parseGenerationCategory :: String -> [String]
parseGenerationCategory text = map (\(_:s:_) -> s) hits 
  where hits = text =~ "\\|#[[:digit:]]+ *\\[\\[([[:alpha:]]+)\\]\\]"  :: [[String]]
