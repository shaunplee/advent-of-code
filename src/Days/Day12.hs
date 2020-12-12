module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Debug.Trace (trace)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 parseInstruction (char '\n')

parseInstruction :: Parser Ins
parseInstruction = do
  ins <- choice ["N", "S", "E", "W", "L", "R", "F"]
  x <- decimal
  return $ case ins of
    "N" -> North x
    "S" -> South x
    "E" -> East x
    "W" -> West x
    "L" -> Lt x
    "R" -> Rt x
    "F" -> Forward x

------------ TYPES ------------
type Input = [Ins]

data Ins
  = North Int
  | South Int
  | East Int
  | West Int
  | Lt Int
  | Rt Int
  | Forward Int
  deriving (Show)

data Ship = Ship {posX :: Int, posY :: Int, heading :: Int}
  deriving (Show)

data Waypoint = Waypoint {wpX :: Int, wpY :: Int}
  deriving (Show)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = abs x + abs y
  where
    Ship x y _ = foldl' (flip updateShip) (Ship 0 0 90) input

updateShip :: Ins -> Ship -> Ship
updateShip (North x) Ship {..} = Ship posX (posY + x) heading
updateShip (South x) Ship {..} = Ship posX (posY - x) heading
updateShip (East x) Ship {..} = Ship (posX + x) posY heading
updateShip (West x) Ship {..} = Ship (posX - x) posY heading
updateShip (Lt x) Ship {..} = Ship posX posY ((heading - x) `mod` 360)
updateShip (Rt x) Ship {..} = Ship posX posY ((heading + x) `mod` 360)
updateShip (Forward x) ship@Ship {..} = case heading of
  0 -> updateShip (North x) ship
  90 -> updateShip (East x) ship
  180 -> updateShip (South x) ship
  270 -> updateShip (West x) ship

------------ PART B ------------
partB :: Input -> OutputB
partB input = abs x + abs y
  where
    ( Ship x y _, _ ) = foldl' (flip updateShipB)
        ( Ship 0 0 90, Waypoint 10 1 ) input

updateShipB :: Ins -> (Ship, Waypoint) -> (Ship, Waypoint)
updateShipB (North x) (ship, Waypoint {..}) = (ship, Waypoint wpX (wpY + x))
updateShipB (South x) (ship, Waypoint {..}) = (ship, Waypoint wpX (wpY - x))
updateShipB (East x) (ship, Waypoint {..}) = (ship, Waypoint (wpX + x) wpY)
updateShipB (West x) (ship, Waypoint {..}) = (ship, Waypoint (wpX - x) wpY)
updateShipB (Rt x) (ship, Waypoint {..}) = case x of
  90 -> (ship, Waypoint wpY (- wpX))
  180 -> (ship, Waypoint (- wpX) (- wpY))
  270 -> (ship, Waypoint (- wpY) wpX)
updateShipB (Lt x) (ship, Waypoint {..}) = case x of
  270 -> (ship, Waypoint wpY (- wpX))
  180 -> (ship, Waypoint (- wpX) (- wpY))
  90 -> (ship, Waypoint (- wpY) wpX)
updateShipB (Forward x) (Ship {..}, wp@Waypoint {..}) =
  (Ship (posX + wpX * x) (posY + wpY * x) heading, wp)
