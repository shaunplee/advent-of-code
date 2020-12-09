module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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
inputParser = Seq.fromList <$> sepBy decimal (char '\n')

------------ TYPES ------------
type Input = Seq Int

type OutputA = Int

type OutputB = Maybe Int

------------ PART A ------------
windowSize = 25

partA :: Input -> Int
partA = go . Seq.splitAt windowSize
  where
    go (_, Seq.Empty) = -1
    go (pre@(p Seq.:<| ps), x Seq.:<| xs) =
      let pairs = [(a,b) | a <- toList pre, b <- toList pre, a /= b]
      in if all (\(a, b) -> a + b /= x) pairs
         then x
         else go (ps Seq.|> x, xs)

------------ PART B ------------
overlappingChunksOf :: Int -> Seq a -> [Seq a]
overlappingChunksOf k xs@(_ Seq.:<| xst) =
  if k > length xs
  then []
  else Seq.take k xs : overlappingChunksOf k xst

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
