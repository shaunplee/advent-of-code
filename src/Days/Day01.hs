module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import Data.Text as T
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
  sepBy' decimal endOfLine


------------ TYPES ------------
type Input = [Integer]

type OutputA = Maybe Integer

type OutputB = Integer

------------ PART A ------------
partA :: Input -> OutputA
partA xs = go (Set.fromList xs)
  where
    go ys = Set.foldr' testMember Nothing ys
      where
        testMember _ (Just v) = Just v
        testMember x Nothing = let xc = (2020 - x)
                               in if Set.member xc ys
                                  then Just $ x * xc
                                  else Nothing



------------ PART B ------------
partB :: Input -> OutputB
partB xs =
  let combs = [(x, y, z) | x <- xs, y <- xs, z <- xs]
      (a, b, c):_ = Data.List.filter (\(x, y, z) -> x + y + z == 2020) combs
  in a * b * c
