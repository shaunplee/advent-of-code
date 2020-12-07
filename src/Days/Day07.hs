module Days.Day07 (runDay) where

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
inputParser = Map.fromList <$> sepBy1 bagRule (char '\n')

bagRule :: Parser BagRule
bagRule = do
  label <- parseLabel
  string " bags contain "
  contents <- parseContents
  char '.'
  return (label, contents)

parseLabel :: Parser String
parseLabel = do
  w1 <- many1 letter
  string " "
  w2 <- many1 letter
  return $ w1 ++ " " ++ w2

parseContents :: Parser (Map String Int)
parseContents =
  choice
    [ do
        string "no other bags"
        return Map.empty,
      Map.fromList
        <$> sepBy1
          ( do
              cnt <- decimal
              char ' '
              label <- parseLabel
              if cnt == 1 then string " bag" else string " bags"
              return (label, cnt)
          )
          (string ", ")
    ]

------------ TYPES ------------
type Input = Map String (Map String Int)

type BagRule = (String, Map String Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA rulesMap = go ["shiny gold"] Set.empty
  where
    bagContents = Map.map Map.keysSet rulesMap
    go [] acc = length acc
    go (k : ks) acc =
      let containing = Map.keys $ Map.filter (Set.member k) bagContents
       in go (ks ++ containing) (Set.union acc (Set.fromList containing))

------------ PART B ------------
partB :: Input -> OutputB
partB rulesMap = go [("shiny gold", 1)] - 1
  where
    go =
      sum
        . map
          ( \(k, n) ->
              n
                * if Map.null $ rulesMap Map.! k
                  then 1
                  else 1 + go (Map.toAscList $ rulesMap Map.! k)
          )
