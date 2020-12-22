module Days.Day22 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Foldable (toList)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence (Seq( (:<|), (:|>)))
import qualified Data.Sequence as Seq
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
  p1 <- playerDeck
  _ <- string "\n\n"
  p2 <- playerDeck
  return (Seq.fromList p1, Seq.fromList p2)

playerDeck :: Parser [Int]
playerDeck = do
  _ <- string "Player " *> digit *> string ":\n"
  sepBy decimal (char '\n')

------------ TYPES ------------
type Input = (Seq Int, Seq Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  let winnerDeck = playGame input
   in score winnerDeck

score :: Seq Int -> Int
score d = sum $ zipWith (*) (toList $ Seq.reverse d) [1 ..]

playGame :: (Seq Int, Seq Int) -> Seq Int
playGame (p1, Seq.Empty) = p1
playGame (Seq.Empty, p2) = p2
playGame (p1, p2) = playGame $ oneRound (p1, p2)

oneRound :: (Seq Int, Seq Int) -> (Seq Int, Seq Int)
oneRound (p1, Seq.Empty) = (p1, Seq.Empty)
oneRound (Seq.Empty, p2) = (Seq.Empty, p2)
oneRound (p1 :<| p1s, p2 :<| p2s) =
  if p1 > p2
    then (p1s :|> p1 :|> p2, p2s)
    else (p1s, p2s :|> p2 :|> p1)

------------ PART B ------------
partB :: Input -> OutputB
partB input = score $ snd $ playRecursiveGame input

playRecursiveGame :: (Seq Int, Seq Int) -> (Bool, Seq Int)
playRecursiveGame (play1, play2) = go (play1, play2) Set.empty
  where
    go (p1, Seq.Empty) _ = (True, p1)
    go (Seq.Empty, p2) _ = (False, p2)
    go s@(p1 :<| p1s, p2 :<| p2s) seen
      | s `Set.member` seen = (True, p1 :<| p1s)
      | p1 <= Seq.length p1s && p2 <= Seq.length p2s =
        if fst $ playRecursiveGame (Seq.take p1 p1s, Seq.take p2 p2s)
          then go (p1s :|> p1 :|> p2, p2s) (Set.insert s seen)
          else go (p1s, p2s :|> p2 :|> p1) (Set.insert s seen)
      | p1 > p2 = go (p1s :|> p1 :|> p2, p2s) (Set.insert s seen)
      | otherwise = go (p1s, p2s :|> p2 :|> p1) (Set.insert s seen)
