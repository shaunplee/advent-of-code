module Days.Day17 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
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
inputParser =
  Map.fromList . concat
    . zipWith (\y -> map (\(x, v) -> ((x, y, 0), v))) [0 ..]
    <$> sepBy1
      parseRow
      (char '\n')

parseRow :: Parser [(Int, Bool)]
parseRow = do
  r <- many1 (char '.' <|> char '#')
  return $ zip [0 ..] (map (== '#') r)

------------ TYPES ------------
type Input = State

type State = Map (Int, Int, Int) Bool

type StateB = Map (Int, Int, Int, Int) Bool

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = length . Map.filter id . (!! 6) $ iterate updateState input

updateState :: State -> State
updateState s =
  Map.fromList $
    map
      ( \p ->
          let n = countNeighbors s p
           in case Map.findWithDefault False p s of
                True -> (p, n == 2 || n == 3)
                False -> (p, n == 3)
      )
      [ (x, y, z)
        | x <- [xmin .. xmax],
          y <- [ymin .. ymax],
          z <- [zmin .. zmax]
      ]
  where
    ((xmin, xmax), (ymin, ymax), (zmin, zmax)) = stateBounds s

stateBounds :: State -> ((Int, Int), (Int, Int), (Int, Int))
stateBounds s =
  let (xs, ys, zs) = unzip3 (Map.keys s)
   in (minMax xs, minMax ys, minMax zs)
  where
    minMax qs = (minimum qs - 1, maximum qs + 1)

countNeighbors :: State -> (Int, Int, Int) -> Int
countNeighbors s p =
  length $
    filter
      (\n -> Map.findWithDefault False n s)
      (neighbors p)

neighbors :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbors (x, y, z) =
  [ (x + dx, y + dy, z + dz)
    | dx <- [-1 .. 1],
      dy <- [-1 .. 1],
      dz <- [-1 .. 1],
      (dx, dy, dz) /= (0, 0, 0)
  ]

------------ PART B ------------
partB :: Input -> OutputB
partB input = length . Map.filter id . (!! 6) $ iterate updateStateB inputB
  where
    inputB =
      Map.fromList $
        map (\((x, y, z), v) -> ((0, x, y, z), v)) $
          Map.toList input

updateStateB :: StateB -> StateB
updateStateB s =
  Map.fromList $
    map
      ( \p ->
          let n = countNeighborsB s p
           in case Map.findWithDefault False p s of
                True -> (p, n == 2 || n == 3)
                False -> (p, n == 3)
      )
      [ (w, x, y, z)
        | w <- [wmin .. wmax],
          x <- [xmin .. xmax],
          y <- [ymin .. ymax],
          z <- [zmin .. zmax]
      ]
  where
    ((wmin, wmax), (xmin, xmax), (ymin, ymax), (zmin, zmax)) =
      stateBoundsB s

stateBoundsB :: StateB -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
stateBoundsB s =
  let (ws, xs, ys, zs) = unzip4 (Map.keys s)
   in (minMax ws, minMax xs, minMax ys, minMax zs)
  where
    minMax qs = (minimum qs - 1, maximum qs + 1)

countNeighborsB :: StateB -> (Int, Int, Int, Int) -> Int
countNeighborsB s p =
  length $
    filter
      (\n -> Map.findWithDefault False n s)
      (neighborsB p)

neighborsB :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
neighborsB (w, x, y, z) =
  [ (w + dw, x + dx, y + dy, z + dz)
    | dw <- [-1 .. 1],
      dx <- [-1 .. 1],
      dy <- [-1 .. 1],
      dz <- [-1 .. 1],
      (dw, dx, dy, dz) /= (0, 0, 0, 0)
  ]
