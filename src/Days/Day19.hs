module Days.Day19 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
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
  rs <- Map.fromList <$> sepBy1 rule (char '\n')
  _ <- endOfLine *> endOfLine
  msgs <- sepBy1 message (char '\n')
  return (rs, msgs)

rule :: Parser (Int, Rule)
rule = do
  k <- decimal
  _ <- string ": "
  v <-
    choice
      [ CharRule <$> (char '"' *> anyChar <* char '"'),
        subrules,
        subrule
      ]
  return (k, v)

subrules :: Parser Rule
subrules = do
  r1 <- sepBy1 decimal (char ' ')
  _ <- string " | "
  r2 <- sepBy1 decimal (char ' ')
  return $ SubRules r1 r2

subrule :: Parser Rule
subrule = SubRule <$> sepBy1 decimal (char ' ')

message :: Parser Message
message = many1' letter

------------ TYPES ------------
type Input = (Rules, [Message])

type Rules = IntMap Rule

data Rule
  = CharRule Char
  | SubRules [Int] [Int]
  | SubRule [Int]
  deriving (Show)

type Message = String

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (rsMap, msgs) = length $ filter (matchRule rsMap 0) msgs

matchRule :: Rules -> Int -> Message -> Bool
matchRule rsMap k msg = case applyRule (rsMap Map.! k) msg of
  Just "" -> True
  _ -> False
  where
    applyRule _ "" = Nothing
    applyRule (CharRule x) (m : ms) = if x == m then Just ms else Nothing
    applyRule (SubRules rs1 rs2) m =
      applyRule (SubRule rs2) m <|> applyRule (SubRule rs1) m
    applyRule (SubRule rules) m =
      foldl'
        ( \ms r -> case ms of
            Nothing -> Nothing
            Just mess -> applyRule (rsMap Map.! r) mess
        )
        (Just m)
        rules

------------ PART B ------------
partB :: Input -> Int
partB (rsMapOld, msgs) =
  length $
    filter
      ( \msg ->
          any
            (\(i, t) -> matchRule rsMap 8 i && matchRule rsMap 11 t)
            (zip (inits msg) (tails msg))
      )
      msgs
  where
    rsMap =
      Map.insert
        11
        (SubRules [42, 31] [42, 11, 31])
        (Map.insert 8 (SubRules [42] [42, 8]) rsMapOld)
