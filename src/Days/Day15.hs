module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
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
inputParser = sepBy decimal (char ',')

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  head $
    Data.List.last $
      Data.List.take (2020 - length input + 1) $
        iterate takeTurn (reverse input)

takeTurn :: [Int] -> [Int]
takeTurn (x : xs) = case elemIndex x xs of
  Nothing -> 0 : x : xs
  Just i -> i + 1 : x : xs

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  snd $
    foldl'
      takeTurnB
      (seen, lst)
      [cnt .. 30000000 - 1]
  where
    cnt = length input
    (front, [lst]) = splitAt (cnt - 1) input
    seen = Map.fromList $ zip front [1 ..]

takeTurnB (sn, l) i = case sn Map.!? l of
  Nothing -> (Map.insert l i sn, 0)
  Just p -> (Map.insert l i sn, i - p)
