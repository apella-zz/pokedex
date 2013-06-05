module Pokedex (module Pokemon,
                module PokeCache,
                module TypeComparison)
                where
import Data.List  (find, union)
import Pokemon -- type definitions
import TypeComparison (typeMap, DamageMult)
import PokeCache
import qualified Data.Map as M (Map, fromList, lookup, mapMaybeWithKey, elems) 

-- | Get a pokemon based on its name
getPokemon :: String -> Pokedex -> Maybe Pokemon
getPokemon pokemon pokedex = find (((==) pokemon) . name) pokedex

------
-- easy access functions:

-- | Get all the pokemon of a given type
getType :: Type -> Pokedex -> Pokedex
getType t = filter ((elem t) . types)

-- | What types have a weak defense against the given type?
weakDefenseAgainst :: Type -> [(Type, DamageMult)]
weakDefenseAgainst t = getTypesAccordingTo (\(a, d) v -> if (t == a) && (v > 1) then Just (d,v) else Nothing)

-- | What types have a strong defense against the given type?
strongDefenseAgainst :: Type -> [(Type, DamageMult)]
strongDefenseAgainst t = getTypesAccordingTo (\(a, d) v -> if (t == a) && (v < 1) then Just (d,v) else Nothing)


-- | Higher order function for getting info on types
getTypesAccordingTo :: ((Type, Type) -> DamageMult -> Maybe a) -> [a]
getTypesAccordingTo fun = M.elems $ M.mapMaybeWithKey fun typeMap

-- | Return the effectiveness of @t1@ 'against' @t2@
against ::  Type -> Type -> DamageMult
t1 `against` t2 = maybe 1 id $ M.lookup (t1, t2) typeMap

-- | What types will do poor against the given type?
weakOffenseAgainst :: Type -> [(Type, DamageMult)]
weakOffenseAgainst t = getTypesAccordingTo (\(a, d) v -> if (t == d) && (v < 1) then Just (a, v) else Nothing)

-- | What types will to well against the given type?
strongOffenseAgainst :: Type -> [(Type, DamageMult)]
strongOffenseAgainst t = getTypesAccordingTo (\(a, d) v -> if (t == d) && (v > 1) then Just (a,v) else Nothing)

-- | Get all the types that have a multiplier against the given type
offenseAgainst :: Type -> [(Type, DamageMult)]
offenseAgainst t = getTypesAccordingTo (\(a, d) v -> if (t == d) then Just (a,v) else Nothing)

-- | Get all the types that have a multiplier against the given type
defenseAgainst :: Type -> [(Type, DamageMult)]
defenseAgainst t = getTypesAccordingTo (\(a, d) v -> if (t == a) then Just (d,v) else Nothing)

-- | Get all the relevant types and their multipliers for attacking
-- the given pokemon
describeDefender :: Pokemon -> [(Type, DamageMult)]
describeDefender =
  foldr union [] .  map offenseAgainst . types


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
