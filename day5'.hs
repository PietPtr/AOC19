import Data.Char
import Debug.Trace

type Left = Int
type Right = Int
type Address = Int
type Value = Int

type Memory = [Int]
type PC = Int

type State = (Memory, PC)

data Moded =
      Immediate Int
    | Position Int
    deriving (Show)

data Instruction =
      Multiply Int Int Int
    | Add Int Int Int
    | Input Int
    | Output Int
    | JumpTrue Int Int
    | JumpFalse Int Int
    | IsLess Int Int Int
    | Equals Int Int Int
    | Halt
    deriving (Show, Eq)

testdec :: Int -> Int -> Int
testdec value position = ord ((padding ++ str) !! position) - 48
    where
        padding = take (5 - length str) $ repeat '0'
        str = show value

parse :: Memory -> PC -> Instruction
parse memory pc = case (opcode) of
    1 -> (Add (makeValue 0) (makeValue 1) (args !! 2))
    2 -> (Multiply (makeValue 0) (makeValue 1) (args !! 2))
    3 -> (Input (args !! 0))
    4 -> (Output (makeValue 0))
    5 -> (JumpTrue (makeValue 0) (makeValue 1))
    6 -> (JumpFalse (makeValue 0) (makeValue 1))
    7 -> (IsLess (makeValue 0) (makeValue 1) (args !! 2))
    8 -> (Equals (makeValue 0) (makeValue 1) (args !! 2))
    99 -> (Halt)
    op -> error ("opcode doesn't exist: " ++ (show op))
    where
        opcode = (testdec decimalInstruction 3) * 10 + (testdec decimalInstruction 4)
        decimalInstruction = memory !! pc

        makeValue index = case (modes !! index) of
            0 -> memory !! (args !! index)
            1 -> args !! index

        args = drop (pc+1) memory
        modes = map (\x -> testdec decimalInstruction x) [2,1,0]

insert :: Memory -> Int -> Int -> Memory
insert memory value destination = memory'
    where
        memory' = before ++ [value] ++ after
        (before,_:after) = splitAt destination memory

operate :: Memory -> Int -> (Int -> Int -> Int) -> Int -> Int -> Memory
operate memory left op right destination = memory'
    where
        memory' = insert memory result destination
        result = op left right

execute :: Instruction -> Memory -> [Int] -> Memory
execute (Multiply left right destination) memory _ = operate memory left (*) right destination
execute (Add left right destination) memory _ = operate memory left (+) right destination
execute (Input destination) memory input = insert memory (input !! 0) destination
execute (IsLess left right destination) memory _ = insert memory (fromEnum (left < right)) destination
execute (Equals left right destination) memory _ = insert memory (fromEnum (left == right)) destination
execute _ memory _ = memory

overwrite :: Int -> Int -> Int
overwrite x y = x

calcpc :: Instruction -> (Int -> Int)
calcpc (Input _) = (2 +)
calcpc (Output _) = (2 +)
calcpc (JumpTrue value destination) = if (value /= 0) then (overwrite destination) else (3 +)
calcpc (JumpFalse value destination) = if (value == 0) then (overwrite destination) else (3 +)
calcpc instr = (4 +)

calcInput :: Instruction -> ([Int] -> [Int])
calcInput (Input _) = drop 1
calcInput instr = id

calcOutput :: Instruction -> ([Int] -> [Int])
calcOutput (Output value) = (++ [value])
calcOutput instr = id

runner :: [Int] -> State -> [Int] -> [Int]
runner input (memory, pc) output = result
    where
        instruction = parse memory pc
        result = if (instruction == (Halt)) then (output) else
            runner input' (memory', pc') output'

        memory' = execute instruction memory input
        pc' = (calcpc instruction) pc
        output' = (calcOutput instruction) output
        input' = (calcInput instruction) input

run source input = runner input (source, 0) []

-- programs

source :: [Int]
source = [1001,4,3,4,33]

echo :: [Int]
echo = [3, 0, 4, 0, 99]

puzzle :: [Int]
puzzle = [3,225,1,225,6,6,1100,1,238,225,104,0,2,218,57,224,101,-3828,224,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1102,26,25,224,1001,224,-650,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,1102,44,37,225,1102,51,26,225,1102,70,94,225,1002,188,7,224,1001,224,-70,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,1101,86,70,225,1101,80,25,224,101,-105,224,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,101,6,91,224,1001,224,-92,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,1102,61,60,225,1001,139,81,224,101,-142,224,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,102,40,65,224,1001,224,-2800,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,1102,72,10,225,1101,71,21,225,1,62,192,224,1001,224,-47,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1101,76,87,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,677,224,102,2,223,223,1005,224,329,1001,223,1,223,1107,677,226,224,102,2,223,223,1006,224,344,1001,223,1,223,7,226,677,224,1002,223,2,223,1005,224,359,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,374,101,1,223,223,108,677,677,224,102,2,223,223,1006,224,389,1001,223,1,223,107,677,226,224,102,2,223,223,1006,224,404,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,419,1001,223,1,223,1107,677,677,224,1002,223,2,223,1006,224,434,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,449,1001,223,1,223,1108,226,677,224,1002,223,2,223,1006,224,464,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,479,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,1008,226,677,224,1002,223,2,223,1005,224,509,1001,223,1,223,1007,677,226,224,102,2,223,223,1005,224,524,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,539,101,1,223,223,1108,226,226,224,1002,223,2,223,1006,224,554,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,569,1001,223,1,223,7,226,226,224,102,2,223,223,1005,224,584,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,614,1001,223,1,223,108,226,226,224,1002,223,2,223,1006,224,629,101,1,223,223,107,677,677,224,102,2,223,223,1005,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,674,1001,223,1,223,4,223,99,226]

test :: Int -> [Int]
test 0 = [3,9,8,9,10,9,4,9,99,-1,8]
test 1 = [3,9,7,9,10,9,4,9,99,-1,8]
test 2 = [3,3,1108,-1,8,3,4,3,99]
test 3 = [3,3,1107,-1,8,3,4,3,99]
test 4 = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
test 5 = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
test 6 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
          1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
          999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
