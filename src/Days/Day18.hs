module Days.Day18 (runDay) where

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
inputParser = sepBy exprt (char '\n')

exprt :: Parser Exprt
exprt = Exprt <$> sepBy expr (char ' ')

expr :: Parser Expr
expr =
  choice
    [ Val <$> decimal,
      Add <$ char '+',
      Mul <$ char '*',
      Parens <$> (char '(' *> exprt <* char ')')
    ]

------------ TYPES ------------
newtype Exprt = Exprt [Expr]
  deriving (Eq)

instance Show Exprt where
  show (Exprt es) = foldr (\e acc -> show e ++ acc) "" es

data Expr
  = Val Int
  | Add
  | Mul
  | Parens Exprt
  deriving (Eq)

instance Show Expr where
  show (Val x) = show x
  show Add = " + "
  show Mul = " * "
  show (Parens e) = "(" ++ show e ++ ")"

type Input = [Exprt]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
testInput :: Text
testInput = "1 + 2 * 3 + 4 * 5 + 6"

partA :: Input -> OutputA
partA = sum . map evalExprt

evalExprt :: Exprt -> Int
evalExprt (Exprt (e : es)) = foldOver (eval e) es

foldOver :: Int -> [Expr] -> Int
foldOver acc [] = acc
foldOver acc (op : y : es) =
  let f = case op of
        Add -> (+)
        Mul -> (*)
   in foldOver (f acc (eval y)) es

eval :: Expr -> Int
eval (Val x) = x
eval (Parens e) = evalExprt e
eval _ = error "cannot eval an operator"

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map evalExprtB

evalExprtB :: Exprt -> Int
evalExprtB (Exprt []) = 0
evalExprtB (Exprt [Val x]) = x
evalExprtB (Exprt [Parens exprt]) = evalExprtB exprt
evalExprtB (Exprt es) = case elemIndex Add es of
  Nothing -> let (x : xs) = es in foldOverB (evalB x) xs
  Just p ->
    let (before, x : _ : y : rest) = splitAt (p - 1) es
     in evalExprtB $ Exprt $ before ++ Val (evalB x + evalB y) : rest

evalB :: Expr -> Int
evalB (Val x) = x
evalB (Parens e) = evalExprtB e
evalB _ = error "cannot eval an operator"

foldOverB :: Int -> [Expr] -> Int
foldOverB acc [] = acc
foldOverB acc (op : y : es) =
  let f = case op of
        Add -> error "should not be any Adds left"
        Mul -> (*)
   in foldOverB (f acc (evalB y)) es
