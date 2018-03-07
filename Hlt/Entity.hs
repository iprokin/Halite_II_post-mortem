-- | Defines Planets and Ships and various functions that operate on them.
module Hlt.Entity where

import qualified Data.Map as Map
import Hlt.Constants
import Text.Read (ReadPrec, readPrec, step)

-- | Grouping of a Player (bot) and its corresponding Ships.
data Player = Player
    { playerId :: PlayerId
    , ships :: Map.Map ShipId Ship
    } deriving (Show)

-- | An Entity is an object on the GameMap.
class Entity t where
    x :: t -> Float
    y :: t -> Float
    radius :: t -> Float

newtype PlayerId = PlayerId Int
    deriving (Eq, Ord)

instance Show PlayerId where
    show (PlayerId i) = show i

newtype ShipId = ShipId Int
    deriving (Eq, Ord)

instance Show ShipId where
    show (ShipId i) = show i

newtype PlanetId = PlanetId Int
    deriving (Eq, Ord)

instance Show PlanetId where
    show (PlanetId i) = show i

data DockingStatus = Undocked | Docking | Docked | Undocking
    deriving (Eq, Show)

data Location = Location
    { locationX :: Float
    , locationY :: Float
    } deriving (Eq, Show)

data Polar = Polar
    { rho   :: Float
    , alpha :: Float
    } deriving (Eq, Show)

data Planet = Planet
    { planetId            :: PlanetId
    , planetX             :: Float
    , planetY             :: Float
    , planetHealth        :: Int
    , planetRadius        :: Float
    , dockingSpots        :: Int
    , production          :: Int
    , remainingResources  :: Int
    , planetOwner         :: Maybe PlayerId
    , dockedShips         :: [ShipId]
    } deriving (Eq, Show)

data Ship = Ship
    { shipId          :: ShipId
    , shipX           :: Float
    , shipY           :: Float
    , shipHealth      :: Int
    , dockingStatus   :: DockingStatus
    , planet          :: Maybe PlanetId
    , dockingProgress :: Int
    , weaponCooldown  :: Int
    , shipOwner       :: Maybe PlayerId
    } deriving (Eq, Show)

instance Entity Location where
    x = locationX
    y = locationY
    radius = const 0

instance Entity Planet where
    x = planetX
    y = planetY
    radius = planetRadius

instance Entity Ship where
    x = shipX
    y = shipY
    radius = pure shipRadius

instance Read PlayerId where
    readPrec = do
      a <- step readPrec :: ReadPrec Int
      return $ PlayerId a

instance Read Player where
    readPrec = do
        i <- step readPrec :: ReadPrec PlayerId -- id
        n <- step readPrec :: ReadPrec Int      -- number of ships
        let next c = if c == 0 then
                         return []
                     else do
                         s <- step readPrec :: ReadPrec Ship
                         ss <- next (c-1)
                         return $ (shipId s, (setShipOwner s i)):ss
        s <- next n                             -- list of ships
        return $ Player i (Map.fromList s)

instance Read ShipId where
    readPrec = do
        a <- step readPrec :: ReadPrec Int
        return $ ShipId a

instance Read DockingStatus where
    readPrec = do
        a <- step readPrec :: ReadPrec Int
        return $ case a of
            0 -> Undocked
            1 -> Docking
            2 -> Docked
            3 -> Undocking

instance Read Ship where
    readPrec = do
        i <- step readPrec :: ReadPrec ShipId        -- id
        j <- step readPrec :: ReadPrec Float         -- x
        k <- step readPrec :: ReadPrec Float         -- y
        h <- step readPrec :: ReadPrec Int           -- health
        _ <- step readPrec :: ReadPrec Float         -- x velocity (deprecated)
        _ <- step readPrec :: ReadPrec Float         -- y velocity (deprecated)
        s <- step readPrec :: ReadPrec DockingStatus -- docking status
        p <- step readPrec :: ReadPrec PlanetId      -- docked planet
        d <- step readPrec :: ReadPrec Int           -- docking progress
        c <- step readPrec :: ReadPrec Int           -- weapon cooldown
        let so = Nothing
        return $ Ship i j k h s (if s /= Undocked then Just p else Nothing) d c so

instance Read PlanetId where
    readPrec = do
        a <- step readPrec :: ReadPrec Int
        return $ PlanetId a

instance Read Planet where
    readPrec = do
        i <- step readPrec :: ReadPrec PlanetId -- id
        j <- step readPrec :: ReadPrec Float    -- x
        k <- step readPrec :: ReadPrec Float    -- y
        h <- step readPrec :: ReadPrec Int      -- health
        r <- step readPrec :: ReadPrec Float    -- radius
        d <- step readPrec :: ReadPrec Int      -- docking spots
        p <- step readPrec :: ReadPrec Int      -- production
        rp <- step readPrec :: ReadPrec Int      -- remaining production (deprecated)
        w <- step readPrec :: ReadPrec Int      -- whether it is owned
        o <- step readPrec :: ReadPrec PlayerId -- the player id if it is owned
        s <- step readPrec :: ReadPrec Int      -- number of docked ships
        let next c = if c == 0 then return []
                     else do
                        s <- step readPrec :: ReadPrec ShipId
                        ss <- next (c-1)
                        return $ s:ss
        l <- next s                             -- list if ship ids
        return $ Planet i j k h r d p rp (if w == 1 then Just o else Nothing) l

setShipOwner :: Ship -> PlayerId -> Ship
setShipOwner (Ship i x y h ds pid dp wc _) playerid = 
    (Ship i x y h ds pid dp wc (Just playerid))

polarRelToLoc :: Entity a => a -> Polar -> Location
polarRelToLoc center (Polar r' a') =
    Location
        { locationX = x center + r' * (cos a')
        , locationY = y center + r' * (sin a')
        }

absLocToPolRel :: Entity a => a -> Location -> Polar
absLocToPolRel center loc = Polar
    { rho   = distance center loc
    , alpha = angleRadians center loc
    }

getLoc :: Entity a => a -> Location
getLoc e = Location (x e) (y e)

-- Check if two Entities are equal
equal :: Entity a => Entity b => a -> b -> Bool
equal a b = (x a == x b && y a == y b)

-- Check if two Entities are not equal
notEqual :: Entity a => Entity b => a -> b -> Bool
notEqual a b = not $ equal a b

-- | Returns the x component of the difference between two Entities.
dX :: Entity a => Entity b => a -> b -> Float
dX a b = x b - x a

-- | Returns the y component of the difference between two Entities.
dY :: Entity a => Entity b => a -> b -> Float
dY a b = y b - y a

-- | Returns the angle in radians between two Entities.
angleRadians :: Entity a => Entity b => a -> b -> Float
angleRadians a b = atan2 (dY a b) (dX a b)

-- | Returns the distance between two Entities.
distance :: Entity a => Entity b => a -> b -> Float
distance a b = sqrt ((dX a b)**2 + (dY a b)**2)

-- | Returns the distance between the edges of two Entities.
distanceEdges :: Entity a => Entity b => a -> b -> Float
distanceEdges a b = distance a b - radius a - radius b

-- | Return the closest Location to an Entity
closestLocationTo :: Entity a => Entity b => a -> b -> Location
closestLocationTo s p = polarRelToLoc p (Polar r' a')
        where a' = angleRadians p s
              r' = radius p + 3

-- | Checks if an Planet is owned by a Player.
isOwned :: Planet -> Bool
isOwned p = planetOwner p /= Nothing

-- | Checks if a Ship is undocked.
isUndocked :: Ship -> Bool
isUndocked s = dockingStatus s == Undocked

-- | Checks if a Ship is docking.
isDocking :: Ship -> Bool
isDocking s = dockingStatus s == Docking

-- | Checks if a Ship is docked.
isDocked :: Ship -> Bool
isDocked s = dockingStatus s == Docked

-- | Checks if a Ship is undocking.
isUndocking :: Ship -> Bool
isUndocking s = dockingStatus s == Undocking

-- | Checks if a Planet is fully docked.
isFull :: Planet -> Bool
isFull p = length (dockedShips p) == dockingSpots p

-- | Checks if a Ship is within docking range of Planet.
canDock :: Ship -> Planet -> Bool
canDock s p = distance s p <= radius p + dockRadius

-- | Checks whether the line segment between a Ship and an Entity collide with a Circle on the GameMap.
isSegmentCircleCollision :: Entity a => Entity b => Entity c => a -> b -> c -> Bool
isSegmentCircleCollision start end circle
    | distAB2 == 0  = distance start circle <= r
    | otherwise     = (t >= 0) && isIntersects
    where
        r = radius circle + collisionFudgeFactor
        isIntersects = distance loc circle <= r
        loc = Location (x start + dx * t) (y start + dy * t)
        dx = dX start end
        dy = dY start end
        distAB2 = dx**2 + dy**2
        partx =
              (x start)**2 - (x start)*(x end) 
            - (x start)*(x circle) + (x end)*(x circle) 
        party =
              (y start)**2 - (y start)*(y end) 
            - (y start)*(y circle) + (y end)*(y circle) 
        k = -2 * ( partx + party )
        t = min (-k / (2 * distAB2)) 1.0
        -- | Constant fudge value so that ships don't collide into to each other.
        collisionFudgeFactor = 1.5
