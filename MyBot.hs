import Hlt.Constants
import Hlt.Entity
import Hlt.GameMap
import Hlt.Networking

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Ord as Ord
import Data.Function (on)

import ListFinite
import Assignment
import General
import HitMap


-- | Define the name of our bot.
botName :: String
botName = "Test"

type ShipsCount = Map.Map PlanetId Int

-- | Log to a file
info :: GameMap -> String -> IO ()
info g s = appendFile (show (myId g) ++ "-" ++ botName ++ ".log") (s ++ "\n")


epsClose :: Entity a => Entity b => Float -> a -> b -> Bool
epsClose eps e = (<=eps) . distance e

filterEntitiesAround :: Entity a => Entity b => Float -> [a] -> b -> [a]
filterEntitiesAround eps entitiesToSearch aroundEntity =
    filter isAround entitiesToSearch
        where isAround = epsClose (radius aroundEntity + eps) aroundEntity


navigateHC :: Entity a => HitMap Int -> Ship -> a -> Assignment
navigateHC hitMap myShip target
  | null good_alphas = stayAssignment myShip--""
  | otherwise        = moveAssign myShip (Polar tv ta)--thrust myShip tv ta
    where
        tv = min maxSpeed d
        ta = argmin (distance target . flip polarL tv) good_alphas
        d = distance myShip target

        good_alphas = filter wayfree alphas

        wayfree a = all ((<=0) . hitMapWO . polarL a) $ rs
        polarL a r = polarRelToLoc myShip (Polar r a)
        hitMapWO = excludeCircleHistogramHitMap 1.5 myShip hitMap
        alphas = [0, pi/90..2*pi] 
        rs = [0,0.2..tv]


navigate :: Entity a => GameMap -> Ship -> a -> Assignment
navigate g myShip target
  | (not.null) good_grid = moveAssign myShip (Polar tv ta)--thrust myShip tv ta
  | otherwise            = stayAssignment myShip--""
    where
        tv = min maxSpeed d
        ta = argmin (distance target . polarL tv) good_grid
        good_grid = filter (not . eb) [0, pi/90..2*pi]
        --eb a = entitiesBetween g True myShip (polarL tv a)
        ps  = filter isClose (listAllPlanets g)
        ss  = filter isClose (listMyShips g)
        isClose e = d + radius e + 1.0 >= distance myShip e
        eb a = entitiesBetweenList ps myShip loc
            || entitiesBetweenList ss myShip loc
            where loc = polarL tv a
        polarL r a = polarRelToLoc myShip (Polar r a)
        d     = distance myShip target


navigateClose :: Entity a => GameMap -> HitMap Int -> Ship -> a -> Assignment
navigateClose g hm s target = navigate g s (closestLocationTo s target)

navigateMission :: Entity a => (a -> Mission) -> GameMap -> HitMap Int -> Ship -> a -> Assignment
navigateMission m g hm s t = setGoal (m t) $ navigateClose g hm s t

navigateMissionP :: Entity a => Mission -> GameMap -> HitMap Int -> Ship -> a -> Assignment
navigateMissionP m g hm s t = setGoal m $ navigate g s t

profit :: GameMap -> ShipsCount -> Ship -> Planet -> Float
profit g sc s p = prft
    where prft = 
              - 3 * log (1 + distance s p)
              + log (radius p)
              + log nFree
              - log (decayToFrom 1.4 0.6 (nMySent / (1+nFree)))
          nFree   = nSpots - nMyDocked
          nSpots  = fromIntegral $ dockingSpots p
          nMyDocked = fromIntegral $ length $ getDockedShipsFromMap (listMyShipsMap g) p
          nMySent   = fromIntegral $
              case Map.lookup (planetId p) sc of
                Just n  -> n
                Nothing -> 0
          --nMy       = nMyDocked + 0.5 * fromIntegral nMySent

isFriendly :: GameMap -> Planet -> Bool
isFriendly g p = owner == Just (myId g) || owner == Nothing
    where owner = planetOwner p

getDockedShipsFromMap :: Map.Map ShipId Ship -> Planet -> [Ship]
getDockedShipsFromMap shipsMap p =
    Maybe.catMaybes $ map (`Map.lookup` shipsMap) (dockedShips p)

getEnemyAround :: Entity a => Float -> GameMap -> a -> [Ship]
getEnemyAround eps g aroundEntity =
    filterEntitiesAround eps enemyShips aroundEntity
        where enemyShips = listEnemyShips g

getMyShipsAround :: Entity a => Float -> GameMap -> a -> [Ship]
getMyShipsAround eps g aroundEntity =
    filterEntitiesAround eps myShips aroundEntity
        where myShips = listMyShips g

findMyPlanetsAround :: Float -> GameMap -> Ship -> [Planet]
findMyPlanetsAround eps g s = filter (epsClose eps s) (listMyPlanets g)

findSpotForSaroundP :: GameMap -> Planet -> Ship -> Maybe Location
findSpotForSaroundP g p s
    | null angleIntervals = Nothing
    | otherwise           = Just $ angleToLocation selAngle
    where selAngle = argmin (dist phyIdeal) (tToL selInterval)
          selInterval = argmin (distToIdeal phyIdeal) angleIntervals
          angleToLocation phy = polarRelToLoc p (Polar r phy)
          angleIntervals = excludeIntervals (0, 2*pi) anglesOfMyShips dPhy
          anglesOfMyShips = map (angleRadians p) myShips
          myShips = filter (not.isUndocked) $ getMyShipsAround (shipRadius + dockRadius) g p
          r = radius p + dockRadius - 1.0
          phyIdeal = angleRadians p s
          distToIdeal best (l, h) = min (dist l best) (dist h best)
          dist x y = abs (x-y)
          --dPhy = acos (1 - 0.5 * (shipRadius / r)**2)
          dPhy = 2 * asin (0.5 * (shipRadius + 0.1) / r)
          tToL (a, b) = [a, b]

threatSize :: GameMap -> Planet -> Float
threatSize g p  = (sum . map attackThreat) enemyShipsAround -- e.g. enemy health in radius
    where attackThreat es = 1.0 / (1.0 + distance p es - radius p)
          enemyShipsAround = getEnemyAround (4 * dockRadius) g p

planetValue :: GameMap -> Planet -> Float
planetValue g p
    | (not.null) myDockedShips = value
    | otherwise                = 0
    where
        myDockedShips = getDockedShipsFromMap (listMyShipsMap g) p
        value =
              (1 + fromIntegral (production p))**0.5
            * (planetRadius p)
            * valueMyDocked
        valueMyDocked = fromIntegral . length $ myDockedShips

findPlanetToProtect :: GameMap -> Ship -> Maybe Planet
findPlanetToProtect g s
    | null around = Nothing
    | otherwise   = Just $ List.maximumBy (Ord.comparing urgence) around
    where urgence p =
              (threatSize g p) 
            * (planetValue g p)
            * (exp $ - (distance p s)/(2 * maxR))
          around  = findMyPlanetsAround (4 * maxR) g s
          maxR    = maximum . map radius $ listAllPlanets g

protectPlanet :: GameMap -> HitMap Int -> Ship -> Planet -> Maybe Assignment
protectPlanet g hm s p
    | null enemyShipsAround || isNotGoodForAttack = Nothing
    | otherwise = Just attackWeakEnemy
    where
        attackWeakEnemy = navigateMission AttackShip g hm s enemy
        isNotGoodForAttack = chosen < log (0.1)
        chosen = evaluateEnemy enemy
        enemy = List.maximumBy (Ord.comparing evaluateEnemy) enemyShipsAround
        evaluateEnemy es = 
            - (((/) `on` fromIntegral) (shipHealth es) maxShipHealth)
            - (distance p es - rP) / (2*rP)
            - (distance s es) / (2*maxSpeed)
        enemyShipsAround = getEnemyAround (4.0 * dockRadius) g p
        rP = radius p

dockOrGo :: GameMap -> HitMap Int -> Ship -> Planet -> Maybe Assignment
dockOrGo g hm s p
  | isOkToDock && not isEnemyAround = Just $ dockDockAssign s p
  | isOkToDock && isEnemyAround     = Just $ dockStayAssign s p
  | not isClose                     = Just $ navigateMission Dock g hm s p
  | isClose && isSpotFound          = Just $ navigateMissionP (Dock p) g hm s spot
  | not isSpotFound                 = Nothing
    where isOkToDock   = canDock s p
          isEnemyAround = not.null $ getEnemyAround (2.5 * maxSpeed) g s
          spot = Maybe.fromJust maybespot
          isSpotFound = Maybe.isJust maybespot
          maybespot = findSpotForSaroundP g p s
          isClose = d <= maxSpeed
          d = distance s p


hitWeakestEnemyDocked :: GameMap -> HitMap Int -> Ship -> Planet -> Assignment
hitWeakestEnemyDocked g hm s p =
    navigateMission AttackShip g hm s weakest
        where
            weakest = List.minimumBy (Ord.comparing shipHealth) enemyDockedShips
            enemyDockedShips = getDockedShipsFromMap (listEnemyShipsMap g) p

dockStatusIndicatior :: Ship -> Float
dockStatusIndicatior s
  | dockingStatus s == Undocked = 0.0
  | otherwise                   = 1.0

statusAdjHealth :: Ship -> Float
statusAdjHealth s =
    (fromIntegral . shipHealth) s
  / (1.0 + 4.0 * dockStatusIndicatior s)

totalHealthA :: [Ship] -> Float
totalHealthA [] = 0
totalHealthA ss = sum . map statusAdjHealth $ ss

totalHealth :: [Ship] -> Float
totalHealth [] = 0
totalHealth ss = sum . map (fromIntegral. shipHealth) $ ss

totalHealthU :: [Ship] -> Float
totalHealthU = totalHealth . filter isUndocked

weakEnemyAround :: Entity a => GameMap -> Bool -> a -> Maybe Ship
weakEnemyAround g fairCount s
  | isGoodForAttack = Just weakest
  | otherwise       = Nothing
    where
        isGoodForAttack = 
               (not.null) enemies
            && (not.null) myShips
            && forceBalance >= 1
        weakest = List.minimumBy (Ord.comparing statusAdjHealth) enemies
        forceBalance = (hMy - hE) / hE
            where
                hMy | fairCount = totalHealthU myShips
                    | otherwise = totalHealthA myShips
                hE  = totalHealthU enemies
        enemies = getEnemyAround eps g s
        myShips = getMyShipsAround eps g s
        eps = maxSpeed * 2.0


weakestClosestEnemy :: Entity a => GameMap -> a -> Maybe Ship
weakestClosestEnemy g s
  | null enemies = Nothing
  | otherwise    = Just weakest
    where
        weakest = List.minimumBy (Ord.comparing healthDist) enemies
        healthDist es = (1.0 + distance s es) * (1.0 + fromIntegral (shipHealth es))
        enemies = listEnemyShips g

commandShipWhenAllMine :: GameMap -> HitMap Int -> Ship -> Assignment
commandShipWhenAllMine g hm s
  | isEnemyAlive = navigateMission AttackShip g hm s weakEC
  | otherwise    = stayAssignment s
  where isEnemyAlive = Maybe.isJust maybeWEC
        maybeWEC = weakestClosestEnemy g s
        weakEC   = Maybe.fromJust maybeWEC

assignShipPlanet :: GameMap -> HitMap Int -> Ship -> Planet -> Maybe Assignment
assignShipPlanet g hm s p
  | isThreatened   = Just $ Maybe.fromJust maybeProtect
  | isWeakAround   = Just $ navigateMission AttackShip g hm s weakA
  | isPlanetFriendly && isOkDockOrGo = Just $ Maybe.fromJust maybeDockOrGo
  | isPlanetFriendly && not isOkDockOrGo = Nothing--stayAssignment s
  | not isPlanetFriendly = Just $ hitWeakestEnemyDocked g hm s p
      where
          isThreatened = (Maybe.isJust maybeP) && (Maybe.isJust maybeProtect)
          isWeakAround = Maybe.isJust maybeWa
          maybeWa = weakEnemyAround g False s
          weakA = Maybe.fromJust maybeWa

          isPlanetFriendly = isFriendly g p

          isOkDockOrGo = Maybe.isJust maybeDockOrGo
          maybeDockOrGo   = dockOrGo g hm s p

          maybeProtect = protectPlanet g hm s (Maybe.fromJust maybeP)
          maybeP       = findPlanetToProtect g s


amIlosing :: GameMap -> Bool
amIlosing g =
       fracOwned > 0.65
    && all (myOwn<=) theirOwn
    && myOwn / maximum theirOwn < 0.25
    && myShipHealth / maximum theirShipHealth < 0.25
        where
            theirOwn        = their mapOwnership
            theirShipHealth = their mapShipHealth
            myOwn = Maybe.fromJust . Map.lookup myid $ mapOwnership :: Float
            myShipHealth = Maybe.fromJust . Map.lookup myid $ mapShipHealth
            myid = myId g
            mapOwnership  = Map.map (fromIntegral . length) (listPlanetsOfPlayers g)
            mapShipHealth = Map.map playerTotalHealth (allPlayers g)
            their = Map.elems . Map.filterWithKey (\k _ -> k /= myid)
            planets = listAllPlanets g
            --lenPlanets = fromIntegral . length $ planets
            nOwned = length . filter ((/=Nothing). planetOwner) $ planets
            fracOwned = fromIntegral nOwned / (fromIntegral . length) planets :: Float
            playerTotalHealth = totalHealthU . Map.elems . ships


assignEscapeShip :: HitMap Int -> HitMap Int -> Ship -> Assignment
assignEscapeShip hitMapCrude hitMapFine myShip =
    navigateHC hitMapFine myShip target
        where
            targetA = pi + meanBy weightedDensity alphas
            target = polarRelToLoc myShip (Polar maxSpeed targetA)
            weightedDensity a =
                sum
                .map (\r -> (fromIntegral . hitMapCrudeWO . polarL a $ r))$
                    [0,2..20*maxSpeed]

            polarL a r = polarRelToLoc myShip (Polar r a)
            hitMapCrudeWO = excludeCircleHistogramHitMap 1.5 myShip hitMapCrude
            alphas = [0, pi/90..2*pi]


assignEscapeShips :: GameMap -> [Ship] -> [Assignment]
assignEscapeShips g myUndockedShips =
    map (assignEscapeShip hitMapCrude hitMapFine) myUndockedShips
        where
            hitMapCrude = buildHitHistogram 0 g [] ss 3 3
            hitMapFine  = buildHitHistogram 1 g ps ss 0.3 0.3
            ps  = listAllPlanets g
            ss  = listAllShips g
            --ess = listEnemyShips g


calcGreatMoves :: ListFinite GameMap Int -> [String]
calcGreatMoves gs
  | amIlosing g = map assToCom . assignEscapeShips g $ myUndockedShips
  | notNull && isAllMine  = map (assToCom . commandShipWhenAllMine g hitMap) myUndockedShips
  | notNull    = map assToCom $ stateMap assPerShip startState shipsPlanets--stateFullMap commandAssStr [] shipsPlanets
  | otherwise  = [""]
    where
        hitMap = undefined--buildHitHistogram 1 g (listAllPlanets g) (listMyShips g) 0.2 0.2
        assPerShip st (s, []) = (st, stayAssignment s)
        assPerShip st (s, ps)
          | Maybe.isJust ass = (newst, Maybe.fromJust ass)
          | otherwise        = assPerShip st (s, tail psR)
            where
                ass = assignShipPlanet g hitMap s p
                newst = Map.insertWith (+) (planetId p) 1 st
                p = head psR
                psR = List.sortBy (flip $ Ord.comparing (profit g st s)) ps
        startState = nshipsPerPlanet
        nshipsPerPlanet = Map.fromList [(planetId p, 0) | p <- planets]

        shipsPlanets = map (\(s, pps) -> (s, map fst pps)) profitSPinit

        profitPlanet s =
            List.sortBy (flip $ Ord.comparing snd)
            [(p, profit g startState s p) | p <- planets]

        profitSPinit =
            List.sortBy (flip $ Ord.comparing (snd.head.snd))
            [(s, profitPlanet s) | s <- myUndockedShips]

        isAllMine = all ((== (Just (myId g))) . planetOwner) planets
        g = headLF gs
        myUndockedShips = filter isUndocked (listMyShips g)
        planets = listAllPlanets g
        notNull = (not.null) planets && (not.null) myUndockedShips


-- | The primary function for controlling the game turns.
run :: ListFinite GameMap Int -> GameMap -> IO ()
run history i = do
    -- Update map
    g <- updateGameMap i
    let newhistory = enlistFinite g history
    info g "---NEW TURN---"
    sendCommands $ calcGreatMoves newhistory
    -- Go to next turn
    run newhistory g


-- | Main function where we initialize our bot and call the run function.
main :: IO ()
main = do
    let history0 = mkListFinite 5 []

    i <- initialGameMap

    -- You can analyse the initial map (i) here, 60 seconds time limit

    sendString botName
    info i ("Initialized bot " ++ botName)
    run history0 i
