module Days.Day14 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.List
import Data.Bits (clearBit, setBit)
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
inputParser = sepBy1 insGroup (char '\n')

insGroup :: Parser InsGroup
insGroup = do
  m <- mask
  _ <- char '\n'
  ins <- sepBy1 store (char '\n')
  return $ InsGroup m ins

mask :: Parser Mask
mask = do
  _ <- string "mask = "
  bs <- many1 (char 'X' <|> char '0' <|> char '1')
  return $
    Mask $
      map
        ( \(c, p) -> case c of
            'X' -> Floating p
            '1' -> One p
            '0' -> Zero p
        )
        (zip bs [35, 34 ..])

store :: Parser Store
store = do
  _ <- string "mem["
  addr <- decimal
  _ <- string "] = "
  val <- decimal
  return $ Store addr val

------------ TYPES ------------
type Input = [InsGroup]

data InsGroup = InsGroup Mask [Store]
  deriving (Show)

newtype Mask = Mask [MaskBit]
  deriving (Show)

data Store = Store {addr :: Int, val :: Int}
  deriving (Show)

data MaskBit
  = One Int
  | Zero Int
  | Floating Int
  deriving (Show)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA ins = sum (foldl' applyIg Map.empty ins)

applyMask :: Mask -> Int -> Int
applyMask (Mask mbs) v =
  foldr
    ( \mb acc -> case mb of
        One k -> setBit acc k
        Zero k -> clearBit acc k
        Floating _ -> acc
    )
    v
    mbs

applyIg :: Map Int Int -> InsGroup -> Map Int Int
applyIg mem (InsGroup mask ms) = foldl' (applyIns mask) mem ms

applyIns :: Mask -> Map Int Int -> Store -> Map Int Int
applyIns mask mem Store {..} = Map.insert addr (applyMask mask val) mem

------------ PART B ------------
partB :: Input -> OutputB
partB ins = sum (foldl' applyIgB Map.empty ins)

applyIgB :: Map Int Int -> InsGroup -> Map Int Int
applyIgB mem (InsGroup mask ms) = foldl' (applyInsB mask) mem ms

applyInsB :: Mask -> Map Int Int -> Store -> Map Int Int
applyInsB mask mem Store {..} =
  foldr (`Map.insert` val) mem (applyMaskB mask addr)

applyMaskB :: Mask -> Int -> [Int]
applyMaskB (Mask mbs) addr =
  foldr
    ( \mb vs -> case mb of
        Zero _ -> vs
        One k -> map (`setBit` k) vs
        Floating k -> concatMap (\v -> [setBit v k, clearBit v k]) vs
    )
    [addr]
    mbs
