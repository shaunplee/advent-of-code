module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
    ( Parser, choice, many', sepBy', char, endOfLine )
import Data.Void

import Debug.Trace (trace)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy' mapRow endOfLine

mapRow :: Parser Row
mapRow = fmap (Vec.fromList . map (== '#')) (many' (choice [char '.', char '#']))
------------ TYPES ------------
type Input = [Row]

type Row = Vector Bool

type OutputA = (Int, Int)

type OutputB = Int

------------ PART A ------------
testInput :: T.Text
testInput = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

partA :: Input -> OutputA
partA rs =
  foldl'
    ( \(pos, cnt) row ->
        let newCnt = if row Vec.! pos then cnt + 1 else cnt
         in ((pos + 3) `mod` width, newCnt)
    )
    (0, 0)
    rs
  where
    width = Vec.length (head rs)

------------ PART B ------------
partB :: Input -> OutputB
partB rs = product [a, b, c, d, e]
  where (_, a) = foldl' (checkRun 1) (0, 0) rs
        (_, b) = foldl' (checkRun 3) (0, 0) rs
        (_, c) = foldl' (checkRun 5) (0, 0) rs
        (_, d) = foldl' (checkRun 7) (0, 0) rs
        (_, e) = foldl' (checkRun 1) (0, 0) (everyOther rs)

checkRun :: Int -> (Int, Int) -> Row -> (Int, Int)
checkRun runLength (pos, cnt) row =
  let newCnt = if row Vec.! pos then cnt + 1 else cnt
   in ((pos + runLength) `mod` width, newCnt)
  where width = length row

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x:_:xs) = x : everyOther xs
