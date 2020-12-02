module Days.Day02 (runDay) where

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
inputParser = sepBy' password endOfLine

password :: Parser Password
password = do
  mn <- decimal
  char '-'
  mx <- decimal
  char ' '
  chr <- anyChar
  string ": "
  pw <- many' letter
  return ((mn, mx), chr, pw)

------------ TYPES ------------
type Password = ((Int, Int), Char, String)

type Input = [Password]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA pwds = length $ filter validPassword pwds

validPassword :: Password -> Bool
validPassword ((mn, mx), chr, pwd) =
  let cnt = length $ filter (== chr) pwd
   in (cnt >= mn) && (cnt <= mx)

------------ PART B ------------
partB :: Input -> OutputB
partB pwds = length $ filter validPasswordB pwds

validPasswordB :: Password -> Bool
validPasswordB ((pos1, pos2), chr, pwd) =
  let p1 = (pwd !! (pos1 - 1)) == chr
      p2 = (pwd !! (pos2 - 1)) == chr
   in (p1 && not p2) || (p2 && not p1)
