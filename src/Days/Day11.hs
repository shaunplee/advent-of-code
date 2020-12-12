module Days.Day11 (runDay) where

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
import Debug.Trace (trace)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  rows <- sepBy parseRow "\n"
  let rrows = zip [0 ..] rows
  let crrows = concatMap (\(r, row) -> map (\(c, v) -> ((r, c), v)) row) rrows
  return $ Map.fromList crrows

parseRow :: Parser [(Int, Maybe Bool)]
parseRow = do
  chrs <- many1 (char 'L' <|> char '#' <|> char '.')
  return $
    zip
      [0 ..]
      ( map
          ( \case
              'L' -> Just False
              '#' -> Just True
              '.' -> Nothing
          )
          chrs
      )

------------ TYPES ------------
type Input = Layout

type Layout = Map (Int, Int) (Maybe Bool)

showLayout :: Layout -> String
showLayout layout =
  [ if c > cmax
      then '\n'
      else case layout Map.! (r, c) of
        Just False -> 'L'
        Just True -> '#'
        Nothing -> '.'
    | r <- [0 .. rmax],
      c <- [0 .. cmax + 1]
  ]
  where
    (rs, cs) = unzip $ Map.keys layout
    rmax = maximum rs
    cmax = maximum cs

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) =
  [ (r + y, c + x) | y <- [-1 .. 1], x <- [-1 .. 1], not (x == 0 && y == 0)
  ]

countNeighbors :: Layout -> (Int, Int) -> Int
countNeighbors layout pos = foldr (\pos acc -> case layout Map.!? pos of
                                      Just (Just True) -> acc + 1
                                      _ -> acc) 0 (neighbors pos)

countOccupied :: Layout -> Int
countOccupied = Map.foldr (\s acc -> case s of
                                       Just True -> acc + 1
                                       _ -> acc) 0

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = go input Set.empty
  where go cur seen = if cur `Set.member` seen
                      then countOccupied cur
                      else go (updateLayout cur) (Set.insert cur seen)

updateLayout :: Layout -> Layout
updateLayout layout =
  Map.mapWithKey
    ( \pos val -> case val of
        Just False ->
          if countNeighbors layout pos == 0
            then Just True
            else Just False
        Just True ->
          if countNeighbors layout pos >= 4
            then Just False
            else Just True
        _ -> val
    )
    layout
------------ PART B ------------
partB :: Input -> OutputB
partB input = go input Set.empty
  where go cur seen = if cur `Set.member` seen
                      then countOccupied cur
                      else go (updateLayoutB cur) (Set.insert cur seen)

countNeighborsB :: Layout -> (Int, Int) -> Int
countNeighborsB layout pos @ ( posr, posc ) = foldr
    (\dir acc -> acc + foldr (checkDirection dir) 0 [ 1 .. ]) 0
    [ ( y, x ) | y <- [ -1 .. 1 ], x <- [ -1 .. 1 ], not (x == 0 && y == 0) ]
  where
    checkDirection ( dirr, dirc ) mul acc
        = case layout Map.!? ( posr + dirr * mul, posc + dirc * mul ) of
            Nothing -> 0
            Just Nothing -> acc
            Just (Just False) -> 0
            Just (Just True) -> 1

updateLayoutB :: Layout -> Layout
updateLayoutB layout =
  Map.mapWithKey
    ( \pos val -> case val of
        Just False ->
          if countNeighborsB layout pos == 0
            then Just True
            else Just False
        Just True ->
          if countNeighborsB layout pos >= 5
            then Just False
            else Just True
        _ -> val
    )
    layout
