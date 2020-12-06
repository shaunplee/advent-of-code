module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
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
inputParser = sepBy1 boardingPass space

boardingPass :: Parser BoardingPass
boardingPass = count 10 anyChar

------------ TYPES ------------
type Input = [BoardingPass]
type BoardingPass = String

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = maximum . map computeBP

computeBP :: BoardingPass -> Int
computeBP bp
    = let ( rowBSP, colBSP ) = splitAt 7 bp
          row = foldl' (\acc c -> acc * 2 + if c == 'F' then 0 else 1) 0 rowBSP
          col = foldl' (\acc c -> acc * 2 + if c == 'L' then 0 else 1) 0 colBSP
    in row * 8 + col

------------ PART B ------------
partB :: Input -> OutputB
partB bps = head $ [mn..mx] \\ takenSeats
  where takenSeats = sort $ map computeBP bps
        mn = head takenSeats
        mx = last takenSeats
