module Days.Day16 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  r <- rules
  _ <- string "\n\nyour ticket:\n"
  yt <- ticket
  _ <- string "\n\nnearby tickets:\n"
  nts <- sepBy1 ticket (char '\n')
  return (r, yt, nts)

rules :: Parser Rules
rules = Map.fromList <$> sepBy rule (char '\n')

rule :: Parser (String, Rule)
rule = do
  k <- many1 (letter <|> space)
  _ <- string ": "
  r1 <- range
  _ <- string " or "
  r2 <- range
  return $ (k, (r1, r2))

range :: Parser (Int, Int)
range = do
  l <- decimal
  _ <- char '-'
  h <- decimal
  return (l, h)

ticket :: Parser Ticket
ticket = sepBy1 decimal (char ',')

------------ TYPES ------------
type Input = (Rules, Ticket, [Ticket])

type Rules = Map String Rule

type Rule = ((Int, Int), (Int, Int))

type Ticket = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (rs, _, nts) = sum (map (ticketErrorRate rs) nts)

notSatisfyRule :: Int -> Rule -> Bool
notSatisfyRule f ((l1, h1), (l2, h2)) =
  not
    ((l1 <= f && f <= h1) || (l2 <= f && f <= h2))

ticketErrorRate :: Rules -> Ticket -> Int
ticketErrorRate rs =
  foldr
    (\f acc -> if all (notSatisfyRule f) rs then acc + f else acc)
    0

------------ PART B ------------
partB :: Input -> OutputB
partB (rs, yt, nts) =
  product $
    map snd $
      filter
        (\(k, _) -> "departure" `isPrefixOf` k)
        (zip fields yt)
  where
    vts = filter (validTicket rs) nts

    fields = deduceFields rs vts

deduceFields :: Rules -> [Ticket] -> [String]
deduceFields rs vts = reduceRules $ map candidateRules (transpose vts)
  where
    candidateRules :: [Int] -> [String]
    candidateRules vs =
      [r | r <- Map.keys rs, all (satisfyRule (rs Map.! r)) vs]
    reduceRules :: [[String]] -> [String]
    reduceRules ss =
      let singles = map head $ filter (\x -> length x == 1) ss
       in if length singles == length ss
            then map head ss
            else
              reduceRules
                ( map
                    ( \s ->
                        if length s == 1
                          then s
                          else filter (`notElem` singles) s
                    )
                    ss
                )

satisfyRule :: Rule -> Int -> Bool
satisfyRule ((l1, h1), (l2, h2)) f =
  (l1 <= f && f <= h1)
    || (l2 <= f && f <= h2)

validTicket :: Rules -> Ticket -> Bool
validTicket rs tckt =
  null
    (foldr (\r t -> filter (not . satisfyRule r) t) tckt (Map.elems rs))
