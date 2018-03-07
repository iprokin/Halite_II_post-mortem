-- | A data type representing the game map at a particular frame, and functions that operate on it.
module Hlt.GameMap where

import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Hlt.Entity

-- | A GameMap contains the current frame of a Halite game.
data GameMap = GameMap
    { myId :: PlayerId
    , width :: Int
    , height :: Int
    , allPlayers :: Map.Map PlayerId Player
    , allPlanets :: Map.Map PlanetId Planet
    } deriving (Show)

-- | Return a list of all Planets.
listAllPlanets :: GameMap -> [Planet]
listAllPlanets g = Map.elems $ allPlanets g

listPlanetsOfPlayers :: GameMap -> Map.Map PlayerId [Planet]
listPlanetsOfPlayers g = Map.fromList [(i, planetsOf i) | i <- pids]
    where pids = map playerId . Map.elems . allPlayers $ g
          planets = listAllPlanets g
          planetsOf i = filter ((==(Just i)) . planetOwner) planets


listMyPlanets :: GameMap -> [Planet]
listMyPlanets g = filter (\p -> planetOwner p == Just (myId g)) . listAllPlanets $ g

-- | Return a list of all Ships.
listAllShips :: GameMap -> [Ship]
listAllShips g = concat $ map (Map.elems . ships) (Map.elems $ allPlayers g)

listEnemyShips :: GameMap -> [Ship]
listEnemyShips = Map.elems . listEnemyShipsMap

listEnemyShipsMap :: GameMap -> Map.Map ShipId Ship
listEnemyShipsMap g = foldl Map.union Map.empty $ map ships (Map.elems enemiesMap)
    where
        enemiesMap =
            Map.filterWithKey (\k _ -> k /= myId g) (allPlayers g)

listAllShipsMap :: GameMap -> Map.Map ShipId Ship
listAllShipsMap g = foldl Map.union Map.empty $ map ships (Map.elems $ allPlayers g)

listMyShipsMap :: GameMap -> Map.Map ShipId Ship
listMyShipsMap g = ships $ Maybe.fromJust $ Map.lookup (myId g) (allPlayers g)

-- | Return a list of my Ships.
listMyShips :: GameMap -> [Ship]
listMyShips = Map.elems . listMyShipsMap

listMyShipsIds :: GameMap -> [ShipId]
listMyShipsIds = Map.keys . listMyShipsMap

-- | Checks if any of the given Entities are in between two Entities.
entitiesBetweenList :: Entity a => Entity b => Entity c => [a] -> b -> c -> Bool
entitiesBetweenList l e0 e1 = 
    any (isSegmentCircleCollision e0 e1) possibleEntities
        where possibleEntities = filter isOk l
              isOk e = notEqual e e0 && notEqual e e1

-- | Checks if there are any Entities between two Entities.
entitiesBetween :: Entity a => Entity b => GameMap -> Bool -> a -> b -> Bool
entitiesBetween g c a b = entitiesBetweenList (listAllPlanets g) a b || (c && entitiesBetweenList (listMyShips g) a b)
