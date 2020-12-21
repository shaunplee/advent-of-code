module Days.Day20 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Text (Text)
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
inputParser = sepBy1 tile (string "\n\n")

tile :: Parser Tile
tile = do
  _ <- string "Tile "
  tileNo <- decimal
  _ <- string ":\n"
  rows <- sepBy parseRow "\n"
  let rrows = zip [0 ..] rows
  let crrows = concatMap (\(r, row) -> map (\(c, v) -> ((r, c), v)) row) rrows
  return $ Tile (tileNo, Map.fromList crrows)

parseRow :: Parser [(Int, Bool)]
parseRow = do
  chrs <- map (== '#') <$> many1 (char '#' <|> char '.')
  return $ zip [0 ..] chrs

parseSeaMonster :: Parser [Pos]
parseSeaMonster = do
  rows <- sepBy1 (zip [0 ..] <$> many1 (char '#' <|> char ' ')) (char '\n')
  let ps =
        concat $
          zipWith
            (\r row -> map (\(c, v) -> ((r, c), v)) row)
            [0 ..]
            rows
  return $ map fst $ filter (\(_, v) -> v == '#') ps

------------ TYPES ------------
type Input = [Tile]

type Pos = (Int, Int)

newtype Tile = Tile (Int, Map Pos Bool)
  deriving (Eq, Ord)

newtype TileMap = TileMap (Map Pos Tile)

instance Show TileMap where
  show (TileMap tm) =
    let ps = groupBy (\x y -> fst x == fst y) $ Map.keys tm
     in concatMap showTileRow ps
    where
      showTileRow :: [Pos] -> String
      showTileRow rps =
        concatMap
          (\r -> concatMap (showRowOfTile r) rts ++ "\n")
          [0 .. td]
          ++ "\n"
        where
          rts = map (tm Map.!) rps

          Tile (_, tmap) = head rts

          td = maximum $ map fst $ Map.keys tmap

          showRowOfTile :: Int -> Tile -> String
          showRowOfTile r (Tile (_, tmap')) =
            [ if tmap' Map.! (r, x) then '#' else '.' | x <- [0 .. td]
            ]
              ++ " "

newtype Border = Border [Bool]
  deriving (Ord, Show)

instance Eq Border where
  (==) (Border b1) (Border b2) = b1 == b2 || b1 == reverse b2

instance Show Tile where
  show (Tile (tid, tmap)) =
    let ps = groupBy (\x y -> fst x == fst y) $ Map.keys tmap
     in "Tile " ++ show tid ++ ":\n"
          ++ concatMap
            ( \row ->
                map (\pos -> if tmap Map.! pos then '#' else '.') row
                  ++ "\n"
            )
            ps

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  let unmatchedBorders =
        nub $
          Map.keys $
            Map.filter
              (== 1)
              (countBorders input)
      tileBorders = map (\t@(Tile (tid, _)) -> (tid, borders t)) input
      unmatchedTileBorders =
        map
          (\(tid, tb) -> (tid, filter (`elem` unmatchedBorders) tb))
          tileBorders
      cornerTiles = filter (\(_, tb) -> length tb == 2) unmatchedTileBorders
   in product $ map fst cornerTiles

canonizeBorder :: Border -> Border
canonizeBorder (Border b) = Border $ min b (reverse b)

borders :: Tile -> [Border]
borders (Tile (_, tm)) =
  map
    Border
    [ map ((tm Map.!) . (0,)) [0 .. 9], -- North
      map ((tm Map.!) . (,0)) [0 .. 9], -- West
      map ((tm Map.!) . (9,)) [0 .. 9], -- South
      map ((tm Map.!) . (,9)) [0 .. 9] -- East
    ]

countBorders :: [Tile] -> Map Border Int
countBorders ts =
  foldr
    ( Map.alter
        ( \case
            Nothing -> Just 1
            Just x -> Just $ x + 1
        )
    )
    Map.empty
    (map canonizeBorder $ concatMap borders ts)

matchingBorder :: Tile -> Tile -> Maybe Border
matchingBorder t1 t2 = case filter (\b -> b `elem` borders t1) (borders t2) of
  [] -> Nothing
  [x] -> Just x
  _ -> error "multiple matching borders"

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let t@(Tile (_, image)) = formImage $ generateMap input
      ts = transforms t
      sms = maximum $ map (\im@(Tile (_, imm)) -> length $ filter (checkSeaMonster im) (Map.keys imm)) ts
   in length (Map.filter id image) - sms * 15

generateMap :: [Tile] -> TileMap
generateMap [] = error "empty input"
generateMap (t : ts) = go (TileMap $ Map.singleton (0, 0) t) (Set.fromList ts)
  where
    go :: TileMap -> Set Tile -> TileMap
    go (TileMap tm) unmatchedTiles
      | Set.null unmatchedTiles = TileMap tm
      | otherwise =
        let frontier =
              filter
                (\p -> length (mapMaybe (tm Map.!?) $ neighbors p) /= 4)
                (Map.keys tm)
            frontierMatches =
              filter (\(_, _, s) -> not $ Set.null s) $
                map
                  ( \fp ->
                      let ft = tm Map.! fp
                       in ( fp,
                            ft,
                            Set.filter
                              ( \t1 -> case matchingBorder t1 ft of
                                  Nothing -> False
                                  Just _ -> True
                              )
                              unmatchedTiles
                          )
                  )
                  frontier
            TileMap ntm =
              TileMap $
                foldr
                  ( \(fp, ft, mts) tmap ->
                      foldr
                        ( \mt tmp ->
                            let Just mb = matchingBorder ft mt
                                ftb = borders ft
                                Just ftp = elemIndex mb ftb
                                Border fb = ftb !! ftp
                                Just mtt =
                                  find
                                    ( \x ->
                                        let xb = borders x
                                            Just xbp = elemIndex mb xb
                                            Border xbb = xb !! xbp
                                         in abs (xbp - ftp) == 2 && fb == xbb
                                    )
                                    $ transforms mt
                             in Map.insert
                                  (sideToPos ftp fp)
                                  mtt
                                  tmp
                        )
                        tmap
                        mts
                  )
                  tm
                  frontierMatches
            ntmTids =
              Set.fromList $
                map (\(Tile (tid, _)) -> tid) $
                  Map.elems ntm
            nut =
              Set.filter
                (\(Tile (tid, _)) -> tid `Set.notMember` ntmTids)
                unmatchedTiles
         in go (TileMap ntm) nut

sideToPos :: Int -> Pos -> Pos
sideToPos 0 (r, c) = (r - 1, c)
sideToPos 1 (r, c) = (r, c - 1)
sideToPos 2 (r, c) = (r + 1, c)
sideToPos 3 (r, c) = (r, c + 1)

neighbors :: Pos -> [Pos]
neighbors (r, c) =
  [ (r + dr, c + dc)
    | dr <- [-1 .. 1],
      dc <- [-1 .. 1],
      abs dc /= abs dr
  ]

transforms :: Tile -> [Tile]
transforms t =
  [ t,
    transformTile rot90 t,
    transformTile rot180 t,
    transformTile rot270 t,
    transformTile flipIm t,
    transformTile (flipIm . rot90) t,
    transformTile (flipIm . rot180) t,
    transformTile (flipIm . rot270) t
  ]

transformTile :: (Map Pos Bool -> Map Pos Bool) -> Tile -> Tile
transformTile f (Tile (tid, tm)) = Tile (tid, f tm)

rot90 :: Map Pos Bool -> Map Pos Bool
rot90 m =
  Map.fromList $
    map
      (\((r, c), v) -> ((c, maxR - r), v))
      $ Map.toAscList m
  where
    maxR = maximum $ map fst $ Map.keys m

rot180 :: Map Pos Bool -> Map Pos Bool
rot180 m =
  Map.fromList $
    map
      (\((r, c), v) -> ((maxR - r, maxR - c), v))
      $ Map.toAscList m
  where
    maxR = maximum $ map fst $ Map.keys m

rot270 :: Map Pos Bool -> Map Pos Bool
rot270 m =
  Map.fromList $
    map
      (\((r, c), v) -> ((maxR - c, r), v))
      $ Map.toAscList m
  where
    maxR = maximum $ map fst $ Map.keys m

flipIm :: Map Pos Bool -> Map Pos Bool
flipIm m =
  Map.fromList $
    map
      (\((r, c), v) -> ((c, r), v))
      $ Map.toAscList m

formImage :: TileMap -> Tile
formImage (TileMap tm) =
  Tile
    (0, Map.fromList $ concatMap localToGlobal (Map.toList tm))
  where
    localToGlobal :: (Pos, Tile) -> [(Pos, Bool)]
    localToGlobal ((tpr, tpc), Tile (_, localMap)) =
      map
        ( \((lpr, lpc), v) ->
            ((lpr + tpr * (td - 1), lpc + tpc * (td - 1)), v)
        )
        ( filter (\((r, c), _) -> not (r == 0 || c == 0 || r == td || c == td)) $
            Map.toList localMap
        )
      where
        td = maximum $ map fst $ Map.keys localMap

seaMonsterString :: Text
seaMonsterString = "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "

seaMonster :: [Pos]
seaMonster = case parseOnly parseSeaMonster seaMonsterString of
  Left e -> error e
  Right sm -> sm

checkSeaMonster :: Tile -> Pos -> Bool
checkSeaMonster (Tile (_, tm)) (pr, pc) = all checkPos seaMonster
  where
    checkPos (smr, smc) = Map.findWithDefault False (pr + smr, pc + smc) tm
