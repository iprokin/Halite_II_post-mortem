-- | Communicate with the Halite game environment.
module Hlt.Networking where

import qualified Data.Map as Map
import System.IO
import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap


-- | Read a list of Players and Ships from a string,
readPlayers :: Int -> String -> (([(PlayerId, Player)], String))
readPlayers 0 s = ([], s)
readPlayers c s = ((playerId a, a):b, r1)
    where (a, r0) = head $ (reads :: ReadS Player) s
          (b, r1) = readPlayers (c-1) r0

-- | Read a list of Planets from a string,
readPlanets :: Int -> String -> (([(PlanetId, Planet)], String))
readPlanets 0 s = ([], s)
readPlanets c s = ((planetId a, a):b, r1)
    where (a, r0) = head $ (reads :: ReadS Planet) s
          (b, r1) = readPlanets (c-1) r0

-- | Get the next int and return the rest of the string.
nextInt :: String -> (Int, String)
nextInt s = head $ (reads :: ReadS Int) s

-- | Read players and planets from stdin.
readGameMap :: IO (Map.Map PlayerId Player,  Map.Map PlanetId Planet)
readGameMap = do
    line <- getLine
    let (c0, r0) = nextInt line
        (p0, r1) = readPlayers c0 r0
        (c1, r2) = nextInt r1
        (p1, _) = readPlanets c1 r2
    return (Map.fromList p0, Map.fromList p1)

-- | Send a string to the game environment.
sendString :: String -> IO ()
sendString s = do
    putStrLn s
    hFlush stdout

-- | Send a list of commands to the game environment.
sendCommands :: [String] -> IO ()
sendCommands = sendString . unwords

-- | Get the initial GameMap.
initialGameMap :: IO GameMap
initialGameMap = do
    line0 <- getLine -- our player id
    line1 <- getLine -- game dimensions
    let i = read line0 :: PlayerId
        (w, h) = (\[a, b] -> (a, b)) $ map read $ words line1
    (p0, p1) <- readGameMap
    return (GameMap i w h p0 p1)

-- | Get the new GameMap.
updateGameMap :: GameMap -> IO GameMap
updateGameMap g = do
    (p0, p1) <- readGameMap
    return (GameMap (myId g) (width g) (height g) p0 p1)
