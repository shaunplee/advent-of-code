module Days.Day06 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 travelGroup (count 2 space)

travelGroup :: Parser TravelGroup
travelGroup = sepBy1 (many1 letter) space
------------ TYPES ------------
type Input = [TravelGroup]
type TravelGroup = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map (length . Set.fromList . concat)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (length . foldr1 Set.intersection . map Set.fromList)
