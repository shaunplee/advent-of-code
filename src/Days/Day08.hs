module Days.Day08 (runDay) where

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
inputParser = Vec.fromList <$> sepBy1 instruction (char '\n')

instruction :: Parser Instruction
instruction = do
  opc <- opCode
  space
  arg <- signed decimal
  return $ PI opc arg

opCode :: Parser OpCode
opCode = do
  opc <- choice [string "acc", string "jmp", string "nop"]
  return $ case opc of
    "acc" -> Acc
    "jmp" -> Jmp
    "nop" -> Nop

------------ TYPES ------------
type Input = Program

type Program = Vector Instruction

data Instruction = PI OpCode Int
  deriving (Show)

data OpCode = Acc | Jmp | Nop
  deriving (Eq, Show)

data MachineState = MachineState {acc :: Int, pc :: Int}

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA prog = go (MachineState 0 0) Set.empty
  where
    go ms@MachineState {..} executed =
      if pc `Set.member` executed
        then acc
        else go (updateState ms) (Set.insert pc executed)
    updateState :: MachineState -> MachineState
    updateState MachineState {..} =
      let (PI op arg) = prog Vec.! pc
       in case op of
            Acc -> MachineState (acc + arg) (pc + 1)
            Jmp -> MachineState acc (pc + arg)
            Nop -> MachineState acc (pc + 1)

------------ PART B ------------
partB :: Input -> OutputB
partB prog =
  head $
    mapMaybe test $
      mapMaybe toggleOp $
        zip
          (replicate (length prog) prog)
          [0 ..]

toggleOp :: (Program, Int) -> Maybe Program
toggleOp (prog, k) =
  let (PI op arg) = prog Vec.! k
   in case op of
        Acc -> Nothing
        Nop -> Just $ prog Vec.// [(k, PI Jmp arg)]
        Jmp -> Just $ prog Vec.// [(k, PI Nop arg)]

test :: Program -> Maybe Int
test prog = go (MachineState 0 0) Set.empty
  where
    go ms@MachineState {..} executed =
      if
          | pc `Set.member` executed -> Nothing
          | pc == length prog -> Just acc
          | otherwise -> go (updateState ms) (Set.insert pc executed)
    updateState :: MachineState -> MachineState
    updateState MachineState {..} =
      let (PI op arg) = prog Vec.! pc
       in case op of
            Acc -> MachineState (acc + arg) (pc + 1)
            Jmp -> MachineState acc (pc + arg)
            Nop -> MachineState acc (pc + 1)
