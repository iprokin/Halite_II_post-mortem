module Assignment where

import qualified Hlt.Entity as E-- (Polar, Ship, Planet)
import Hlt.Navigation

data Mission = AttackPlanet E.Planet | AttackShip E.Ship | Dock E.Planet | Idle deriving Eq
data Command = DockTo E.Planet | MoveTo E.Polar | Stay

data Assignment = Assignment
    { cmd         :: Command
    , goal        :: Mission
    , ship        :: E.Ship
    }

dockDockAssign :: E.Ship -> E.Planet -> Assignment
dockDockAssign s p = Assignment
    { cmd         = DockTo p
    , goal        = Dock p
    , ship        = s
    }

dockStayAssign :: E.Ship -> E.Planet -> Assignment
dockStayAssign s p = Assignment
    { cmd         = Stay
    , goal        = Dock p
    , ship        = s
    }

moveAssign :: E.Ship -> E.Polar -> Assignment
moveAssign s l = Assignment {cmd=MoveTo l, goal=Idle, ship=s}

setGoal :: Mission -> Assignment -> Assignment
setGoal m a = a {goal=m}

stayAssignment :: E.Ship -> Assignment
stayAssignment s = Assignment {goal=Idle, cmd=Stay, ship=s}

assToCom :: Assignment -> String
assToCom (Assignment { cmd = DockTo p, ship = s }) = dock s p
assToCom (Assignment { cmd = MoveTo (E.Polar tv ta)
                     , ship = s
                     }) = thrust s tv ta
assToCom (Assignment { cmd = Stay }) = ""
