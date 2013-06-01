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
                  } deriving (Eq, Show)

-- | The type of a pokemon
data Type = Normal | Flying
            deriving (Eq, Show)


-- | Basic definition of a pokemon
data Pokemon = Pokemon { name :: String
                       , types :: [Type] -- | Pokemon can have multiple types. 
                       , dex  :: Int -- | The national pokedex id
                       } deriving (Eq, Show)

