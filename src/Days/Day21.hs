module Days.Day21 (runDay) where

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
inputParser = sepBy1 food (char '\n')

food :: Parser Food
food = do
  ings <- sepBy1 (many1 letter) (char ' ')
  _ <- string " (contains "
  as <- sepBy (many1 letter) (string ", ")
  _ <- char ')'
  return $ Food ings as

------------ TYPES ------------
type Input = [Food]

type Ingredient = String

type Allergen = String

data Food = Food [Ingredient] [Allergen]
  deriving (Show)

type OutputA = Int

type OutputB = String

------------ PART A ------------
partA :: Input -> OutputA
partA input = sum $ map (\i -> length $ filter (== i) allIngredients)
    allergenFree
  where
    containsAllergens
        = concat $ Map.elems $ reduceCandidates $ initialCandidates input

    ingredients = Set.toList $ Set.fromList $ concatMap (\(Food is _) -> is)
        input

    allIngredients = concatMap (\(Food is _) -> is) input

    allergenFree = ingredients \\ containsAllergens

initialCandidates :: Input -> Map Allergen [Ingredient]
initialCandidates fs =
  Map.fromList $
    map
      computeIngredientCandidates
      (Set.toList allergens)
  where
    allergens = Set.fromList $ concatMap (\(Food _ as) -> as) fs

    matchingFoods a = filter (\(Food _ as) -> a `elem` as) fs

    ings foods = map (Set.fromList . (\(Food ins _) -> ins)) foods

    computeIngredientCandidates a =
      (a, Set.toList $ foldr1 Set.intersection (ings (matchingFoods a)))

reduceCandidates :: Map Allergen [Ingredient] -> Map Allergen [Ingredient]
reduceCandidates ais =
  let known = concat $ filter (\ins -> length ins == 1) (Map.elems ais)
      newAis = Map.map (\is -> if length is > 1 then is \\ known else is) ais
   in if newAis == ais then ais else reduceCandidates newAis

------------ PART B ------------
partB :: Input -> OutputB
partB input = intercalate "," containsAllergens
  where
    containsAllergens
        = concat $ Map.elems $ reduceCandidates $ initialCandidates input
