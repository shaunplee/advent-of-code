module Days.Day10 (runDay) where

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
inputParser = sepBy decimal (char '\n')

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  let jas = sort (0 : (3 + maximum input) : input)
      (oneDiffs, threeDiffs) =
        foldr
          ( \(x, y) (ones, threes) ->
              let diff = y - x
               in if
                      | diff == 1 -> (ones + 1, threes)
                      | diff == 3 -> (ones, threes + 1)
                      | otherwise -> (ones, threes)
          )
          (0, 0)
          (zip jas (tail jas))
   in trace (show oneDiffs ++ "," ++ show threeDiffs) (oneDiffs * threeDiffs)

------------ PART B ------------
partB :: Input -> OutputB
partB input = go (Map.fromList [(-2, 0), (-1, 0), (0, 1)]) jas
  where
    jas = sort input
    go visited [la] = countWays visited la
    go visited (a : as) = go (Map.insert a (countWays visited a) visited) as
    countWays visited a =
      sum $
        map
          (\k -> Map.findWithDefault 0 (a - k) visited)
          [1 .. 3]
