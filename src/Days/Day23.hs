module Days.Day23 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
inputParser = Seq.fromList . map digitToInt <$> many1 digit

------------ TYPES ------------
type Input = Seq Int

type OutputA = String

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  let (_, cups) = iterate oneMove (Seq.index input 0, input) !! 100
      Just oneIdx = Seq.elemIndexL 1 cups
      (back, front) = Seq.splitAt oneIdx cups
   in tail $ concatMap show (front >< back)

oneMove :: (Int, Seq Int) -> (Int, Seq Int)
oneMove (c, cups) =
  let Just cPos = Seq.elemIndexL c cups
      (before, move, after) = pickupCups cPos cups
      nCups = before >< after
      nextCup x =
        let nx = if x == 1 then maximum nCups else x - 1
         in case Seq.elemIndexL nx nCups of
              Just dP -> dP
              Nothing -> nextCup (x - 1)
      dPos = nextCup c
      (before', after') = Seq.splitAt (dPos + 1) nCups
      cups' = before' >< move >< after'
      Just ncPos = Seq.elemIndexL c cups'
      newCurPos = if ncPos + 1 == length cups then 0 else ncPos + 1
   in (Seq.index cups' newCurPos, cups')

pickupCups :: Int -> Seq Int -> (Seq Int, Seq Int, Seq Int)
pickupCups pos cups
  | cupsAfter >= 3 =
    let (before, next) = Seq.splitAt (pos + 1) cups
        (move, rest) = Seq.splitAt 3 next
     in (before, move, rest)
  | cupsAfter < 3 =
    let (before, fstMove) = Seq.splitAt (pos + 1) cups
        (restMove, before') = Seq.splitAt (3 - cupsAfter) before
     in (before', fstMove >< restMove, Seq.Empty)
  | otherwise = error "pos is beyond range of cups"
  where
    lcups = length cups
    cupsAfter = lcups - pos - 1

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let (_, cups) = iterate oneMove (Seq.index input 0, input >< Seq.fromList [10..1000000]) !! 10000000
      Just oneIdx = Seq.elemIndexL 1 cups
      x = Seq.index cups (oneIdx + 1)
      y = Seq.index cups (oneIdx + 2)
   in x * y
