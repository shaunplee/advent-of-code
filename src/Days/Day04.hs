module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Char (isSpace)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
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
inputParser = sepBy1 passport (count 2 space)

passport :: Parser Passport
passport = fmap Map.fromList (sepBy1 keyValue space)

keyValue :: Parser KeyValue
keyValue = do
  k <- Data.Attoparsec.Text.take 3
  char ':'
  v <- takeTill isSpace
  return (k, v)

------------ TYPES ------------
type Input = [Passport]

type Passport = Map Text Text

type KeyValue = (Text, Text)

data Height = Cm Int | In Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter validPassport

passportFields = ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]

npdFields = ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]

validPassport :: Passport -> Bool
validPassport pp =
  let fields = Map.keys pp
   in fields == passportFields || fields == npdFields

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter validPassportB

validPassportB :: Passport -> Bool
validPassportB pp = validPassport pp && validFields
  where
    validFields = all (\k -> (validators Map.! k) (pp Map.! k)) (Map.keys pp)

validators :: Map Text (Text -> Bool)
validators = Map.fromList
    [ ( "byr", validByr )
    , ( "ecl", validEcl )
    , ( "eyr", validEyr )
    , ( "hcl", validHcl )
    , ( "hgt", validHgt )
    , ( "iyr", validIyr )
    , ( "pid", validPid )
    , ( "cid", const True)
    ]

validByr :: Text -> Bool
validByr yr = case parseOnly (decimal <* endOfInput) yr of
  Left _ -> False
  Right year -> year >= 1920 && year <= 2002

validIyr :: Text -> Bool
validIyr yr = case parseOnly (decimal <* endOfInput) yr of
  Left _ -> False
  Right year -> year >= 2010 && year <= 2020

validEyr :: Text -> Bool
validEyr yr = case parseOnly (decimal <* endOfInput) yr of
  Left _ -> False
  Right year -> year >= 2020 && year <= 2030

validHgt :: Text -> Bool
validHgt hgt = case parseOnly (height <* endOfInput) hgt of
  Left _ -> False
  Right (Cm ht) -> ht >= 150 && ht <= 193
  Right (In ht) -> ht >= 59 && ht <= 76

height :: Parser Height
height = do
  ht <- decimal
  unit <- choice [string "cm", string "in"]
  return (if unit == "cm" then Cm ht else In ht)

validHcl :: Text -> Bool
validHcl hcl = case parseOnly
  (char '#' *> count 6 (satisfy $ inClass "0-9a-f") <* endOfInput)
  hcl of
  Left _ -> False
  Right _ -> True

validEcl :: Text -> Bool
validEcl =
  flip Set.member $
    ( Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] ::
        Set Text
    )

validPid :: Text -> Bool
validPid pid = case parseOnly (count 9 digit <* endOfInput) pid of
  Left _ -> False
  Right _ -> True
