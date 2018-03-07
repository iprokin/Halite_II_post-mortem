module HitMap where

import Hlt.Entity
import Hlt.GameMap
import Hlt.Constants

import ListFinite
import Data.Array (array, (!))

import General (symAngle)

type GameMapHistory = ListFinite GameMap Int

type HitMap a = (Location -> a)
type HitMapPolar a = (Polar -> a)

-- Smooth hit map

{-
x' y' rho' alpha' - coordinates relative to entity e
that is x' = x - x_e

All kernels are defined in relative coordinates
-}
kernelShip :: Float -> p -> Float -> Float -> Float
kernelShip bstd rstd beta' rho'
    | rho' <= rhoMax = angleK * rhoK
    | otherwise      = 0
    where angleK
            | bstd /= 0 = exp (-0.5*((symAngle beta')/bstd)**2)
            | otherwise = 1.0
          safeExp rho' = exp (-(rho'-safetyR)/lenRho)
              where safetyR = 0.1 + shipRadius
          lenRho = maxSpeed*0.3
          rhoMax = lenRho * 4
          rhoK = safeExp rho'--1/(2*pi*s*s) * exp (-0.5*(rho/s)**2) * rho * dV
          --    where s  = 2*shipRadius
          --          dV = 2*pi/60
          --  | rstd /= 0 = rho * exp (-0.5*(rho/rstd)**2) / k
          --  | otherwise = safeExp rho
          --k      = rstd**2 * 0.607

kernelPlanet :: Fractional p => Float -> Float -> p
kernelPlanet radiusP rho'
    | rho' <= radiusP + safeMargin = 1.0
    | otherwise                    = 0.0
    where safeMargin = shipRadius + 0.5

kForShip :: (Entity e1, Entity e2) => e1 -> e2 -> Float
kForShip point s = kernelShip 0 0 beta' rho'
    where beta' = symAngle $ angleRadians s point
          rho'  = distance s point

kForPlanet :: (Entity e1, Entity e2, Fractional p) => e1 -> e2 -> p
kForPlanet point p = kernelPlanet (radius p) (distance p point)

buildHitMap :: GameMapHistory -> HitMap Float
buildHitMap gs = hitmap
    where hitmap (Location x y) = arr ! ((fromC x), (fromC y))
          arr = array ((0,0), (iw, ih))
            [((i,j), lscape i j) | i <- is, j <- js]

          lscape i j = shipScape + planetScape
              where
                  shipScape   = sum . map (kForShip point) $ shipsNow
                  planetScape = sum . map (kForPlanet point) $ planetsNow
                  point = Location (toC i) (toC j)

          shipsNow   = listAllShips now
          planetsNow = listAllPlanets now
          w = width now
          h = height now
          now = headLF gs
          fromC x = round (x / precision)
          toC i   = precision * (fromIntegral i)
          iw = fromC (fromIntegral w) :: Int
          ih = fromC (fromIntegral h) :: Int
          precision = 0.25
          is = [0..iw]
          js = [0..ih]


buildHitMapLocal :: Entity a => GameMapHistory -> Ship -> a -> HitMap Float
buildHitMapLocal gs myShip target = hitmap
    where hitmap point = shipScape + planetScape
              where
                  shipScape   = sum . map (kForShip point)   $ shipsNow
                  planetScape = sum . map (kForPlanet point) $ planetsNow
          shipsNow   = filter isNotSrcOrDest . listAllShips   $ now
          planetsNow = listAllPlanets now--filter isNotSrcOrDest . listAllPlanets $ now
          isNotSrcOrDest e = notEqual myShip e && notEqual target e
          now = headLF gs


-- Histogram hit map

buildHitHistogram :: Int -> GameMap -> [Planet] -> [Ship] -> Float -> Float -> HitMap Int
buildHitHistogram wall g ps ss dx dy = hitmap
    where
        hitmap (Location x y)
            | isBorder = wall
            | otherwise = arr ! ((fromC dx x), (fromC dy y))
            where isBorder = x < 0 || x > w || y < 0 || y > h
        arr = array ((0,0), (iw, ih))
            [((i,j), lscape i j) | i <- is, j <- js]

        lscape :: Int -> Int -> Int
        lscape i j = countInBox ss i j + countInBox ps i j
        countInBox es i j = sum . map (fromEnum . isInBox i j) $  es

        --isInBox :: Entity a => Int -> Int -> a -> Bool
        --isInBox i j e = 
        --       (x0    < x e + r)
        --    && (x0+dx > x e - r)
        --    && (y0    < y e + r)
        --    && (y0+dy > y e - r)
        --    where x0 = toC dx i
        --          y0 = toC dy j
        --          r = radius e
        
        isInBox i j e = 
               isSegmentCircleCollision
                    (Location x0 y0) (Location (x0+dx) (y0+dy)) e
            || isSegmentCircleCollision
                    (Location x0 (y0+dy)) (Location (x0+dx) y0) e
            where x0 = toC dx i
                  y0 = toC dy j
            

        fromC dz z = truncate (z / dz)
        toC dz k   = dz * (fromIntegral k)
        iw = fromC dx w
        ih = fromC dy h
        is = [0..iw]
        js = [0..ih]
        w = fromIntegral $ width g
        h = fromIntegral $ height g

-- Hit map transformers

hmToPolarRelHm :: Entity a => a -> HitMap b -> HitMapPolar b
hmToPolarRelHm center hm = hm . polarRelToLoc center

excludeCircleHistogramHitMap :: Num a => Entity b => Float -> b -> HitMap a -> HitMap a
excludeCircleHistogramHitMap collisionFudgeFactor e hitmap = newHitmap
    where newHitmap l
            | distance e l  <= r**2 = hitmap l - 1
            | otherwise             = hitmap l
            where r = radius e + collisionFudgeFactor


hitMapWithoutMe :: Ship -> HitMap Float -> HitMap Float
hitMapWithoutMe myShip hitMap = newHitmap
    where newHitmap loc = hitMap loc - (kForShip loc myShip)
