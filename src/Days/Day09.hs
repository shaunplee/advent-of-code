module Days.Day09 (runDay) where

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
inputParser = sepBy decimal (char '\n')

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Maybe Int

------------ PART A ------------
windowSize = 25

partA :: Input -> Int
partA = go . splitAt windowSize
  where
    go (_, []) = -1
    go (pre, x : xs) = let pairs = [(a,b) | a <- pre, b <- tail pre, a /= b]
                       in if all (\(a, b) -> a + b /= x) pairs
                          then x
                          else go (tail pre ++ [x], xs)

------------ PART B ------------
overlappingChunksOf :: Int -> [a] -> [[a]]
overlappingChunksOf k xs =
  if k > length xs
  then []
  else Data.List.take k xs : overlappingChunksOf k (tail xs)

partB :: Input -> OutputB
partB input = foldr go Nothing [3 .. length input]
  where
    val = partA input
    go _ (Just x) = Just x
    go window Nothing = foldr go' Nothing (overlappingChunksOf window input)
    go' _ (Just x) = Just x
    go' chunk Nothing = if sum chunk == val
      then Just (minimum chunk + maximum chunk)
      else Nothing
