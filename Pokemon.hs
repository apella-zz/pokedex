-- | This module provides the basic types for pokemon 
module Pokemon where

-- | the base stats of a pokemon
data Stat = Stats { hp :: Int
                  , atk :: Int
                  , def :: Int
                  , satk :: Int
                  , sdef :: Int
                  , spd :: Int
                  , total :: Int
                  } deriving (Eq, Show, Read)

-- | The type of a pokemon
data Type = Normal
          | Fire
          | Water
          | Grass
          | Electric
          | Ice
          | Fighting
          | Poison
          | Ground
          | Flying
          | Psychic
          | Bug
          | Rock
          | Ghost
          | Dragon
          | Dark
          | Steel
          deriving (Eq, Show, Read, Ord)


-- | Basic definition of a pokemon
data Pokemon = Pokemon { name :: String
                       , types :: [Type] -- | Pokemon can have multiple types. 
                       , dex  :: Int -- | The national pokedex id
                       , stats :: Stat
                       , generation :: Int 
                       } deriving (Eq, Show, Read)

