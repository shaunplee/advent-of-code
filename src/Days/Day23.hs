module Days.Day23 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.Maybe
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
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
inputParser = map digitToInt <$> many1 digit

------------ TYPES ------------
type Input = [Int]

type OutputA = String

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  let initCups = Map.fromList $ zip input $ drop 1 (cycle input)
      (_, cups) = iterate oneMove (head input, initCups) !! 100
      result =
        until
          (\(x : xs) -> x == 1)
          (\(x : xs) -> (cups Map.! x) : x : xs)
          [cups Map.! 1]
   in concatMap show (reverse $ tail result)

oneMove :: (Int, IntMap Int) -> (Int, IntMap Int)
oneMove (c, cups) =
  let ((first, second, third), after) = pickupCups c cups
      move = [first, second, third]
      dest = destCup (c - 1)
      destCup x
        | x == 0 = destCup (maximum cups)
        | x `elem` move = destCup (x - 1)
        | otherwise = x
      cupAfterDest = cups Map.! dest
      newCups = foldl' (\cm (k, v) -> Map.insert k v cm) cups [(dest, first), (third, cupAfterDest), (c, after)]
   in (newCups Map.! c, newCups)

pickupCups :: Int -> IntMap Int -> ((Int, Int, Int), Int)
pickupCups curCup cups =
  let first = cups Map.! curCup
      second = cups Map.! first
      third = cups Map.! second
      fourth = cups Map.! third
   in ((first, second, third), fourth)

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let inputExt = input ++ [10..1000000]
      initCups = Map.fromList $ zip inputExt $ drop 1 (cycle inputExt)
      (_, cups) = iterate oneMove (head input, initCups) !! 10000000
      x = cups Map.! 1
      y = cups Map.! x
   in x * y
