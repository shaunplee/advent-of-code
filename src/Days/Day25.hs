module Days.Day25 (runDay) where

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
inputParser = do
  x <- decimal
  _ <- char '\n'
  y <- decimal
  return (x, y)

------------ TYPES ------------
type Input = (Int,Int)

type OutputA = Int

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA (cpk, dpk) = let cl = findLoopSize cpk
                       dl = findLoopSize dpk
                   in iterate (oneLoop dpk) 1 !! cl

subjectNumberA :: Int
subjectNumberA = 7

oneLoop :: Int -> Int -> Int
oneLoop subjectNumber v = (v * subjectNumber) `mod` 20201227

findLoopSize :: Int -> Int
findLoopSize pk = snd $ head $ dropWhile (\(x, _) -> x /= pk) (zip (iterate (oneLoop subjectNumberA) 1) [0..])

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
