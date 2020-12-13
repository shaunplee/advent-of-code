module Days.Day13 (runDay) where

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
import Debug.Trace (trace)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  est <- decimal
  _ <- char '\n'
  busses <- sepBy1 (choice [many1 digit, many1 (char 'x')]) (char ',')
  return
    ( est,
      map
        ( \case
            "x" -> Nothing
            dgts -> Just (read dgts)
        )
        busses
    )

------------ TYPES ------------
type Input = (Integer, [Maybe Integer])

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
partA :: Input -> OutputA
partA (est, busses) = t * bid
  where
    nextTime bus = bus - (est `mod` bus)
    (t, bid) = minimum $ map (\x -> (nextTime x, x)) (catMaybes busses)

------------ PART B ------------
partB :: Input -> OutputB
partB (_, busses) = fst $ foldl' findTs (firstTs, firstBid) (tail busBids)
  where
    busBids =
      catMaybes $
        zipWith
          ( \mb k -> case mb of
              Nothing -> Nothing
              Just bid -> Just (bid, k)
          )
          busses
          [0 ..]
    (firstBid, _) = head busBids
    firstTs = (0 `div` firstBid) * firstBid + firstBid

findTs :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
findTs (curTs, lcm) (bid, pos) =
  if (curTs + pos) `mod` bid == 0
    then (curTs, lcm * bid)
    else findTs (curTs + lcm, lcm) (bid, pos)

partB_old :: Input -> OutputB
partB_old (_, busses) =
  fst $
    foldr
      ( \(nbid, npos) (ts, lcm) ->
          foldr
            ( \k (t, l) ->
                if (t + k) `mod` nbid == npos
                  then (t + k, l * nbid)
                  else (ts, lcm)
            )
            (ts, lcm)
            [0, lcm ..]
      )
      (firstTs, firstBid)
      (tail busBids)
  where
    -- foldr
    --   (\ts acc -> if all (checkBus ts) shiftedBusBids then ts - maxPos else acc)
    --   0
    --   [firstTs, firstTs + lcm ..]

    busBids =
      catMaybes $
        zipWith
          ( \mb k -> case mb of
              Nothing -> Nothing
              Just bid -> Just (bid, k)
          )
          busses
          [0 ..]

    (firstBid, firstPos) = head busBids

    (maxBid, maxPos) = trace (show busBids) $ maximum busBids

    shiftedBusBids = map (\(bid, pos) -> (bid, pos - maxPos)) busBids

    lcm = product $ map fst busBids

    firstTs = (100000000000000 `div` maxBid) * maxBid + maxBid

    -- firstTs = (1000000 `div` maxBid) * maxBid + maxBid
    checkBus baseTs (bid, pos) = (baseTs + pos) `mod` bid == 0
