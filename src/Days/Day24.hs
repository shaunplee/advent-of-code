module Days.Day24 (runDay) where

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
inputParser = sepBy1 tile (char '\n')

tile :: Parser [Dir]
tile = many1 dir

dir :: Parser Dir
dir =
  choice
    [ E <$ string "e",
      SE <$ string "se",
      SW <$ string "sw",
      W <$ string "w",
      NW <$ string "nw",
      NE <$ string "ne"
    ]

------------ TYPES ------------
type Input = [Tile]

type Tile = [Dir]

data Dir = E | SE | SW | W | NW | NE
  deriving (Eq, Show)

newtype Pos = Pos (Int, Int, Int)
  deriving (Eq, Ord, Show)

instance Semigroup Pos where
  (Pos (x1, y1, z1)) <> (Pos (x2, y2, z2)) = Pos (x1 + x2, y1 + y2, z1 + z2)

instance Monoid Pos where
  mempty = Pos (0, 0, 0)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = length $ Map.filter odd $ initialState input

initialState :: Input -> Map Pos Int
initialState = foldr (\t -> Map.insertWith (+) (tileToPos t) 1) Map.empty

tileToPos :: Tile -> Pos
tileToPos = mconcat . map dirToPos

dirToPos :: Dir -> Pos
dirToPos W = Pos (-1, 1, 0)
dirToPos SW = Pos (-1, 0, 1)
dirToPos SE = Pos (0, -1, 1)
dirToPos E = Pos (1, -1, 0)
dirToPos NE = Pos (1, 0, -1)
dirToPos NW = Pos (0, 1, -1)

------------ PART B ------------
partB :: Input -> OutputB
partB input = map countBlackTiles (iterate oneDay $ initialState input) !! 100

countBlackTiles :: Map Pos Int -> Int
countBlackTiles = length . Map.filter odd

oneDay :: Map Pos Int -> Map Pos Int
oneDay cur =
  let bt = blackTiles cur
      wt = Set.fromList $ concatMap neighbors bt
      blackFlipped =
        foldr
          ( \p m ->
              let bn = countBlackNeighbors cur p
               in Map.insertWith
                    (+)
                    p
                    (if bn == 0 || bn >= 2 then 1 else 0)
                    m
          )
          cur
          bt
   in foldr
        ( \p m ->
            let bn = countBlackNeighbors cur p
             in Map.insertWith (+) p (if bn == 2 then 1 else 0) m
        )
        blackFlipped
        wt

countBlackNeighbors :: Map Pos Int -> Pos -> Int
countBlackNeighbors m p =
  length $
    filter odd $
      map
        (\k -> Map.findWithDefault 0 k m)
        (neighbors p)

neighbors :: Pos -> [Pos]
neighbors p =
  map
    (p <>)
    [ Pos (-1, 1, 0),
      Pos (-1, 0, 1),
      Pos (0, -1, 1),
      Pos (1, -1, 0),
      Pos (1, 0, -1),
      Pos (0, 1, -1)
    ]

blackTiles :: Map Pos Int -> [Pos]
blackTiles = Map.keys . Map.filter odd

dToP :: Dir -> Pos
dToP d = _
