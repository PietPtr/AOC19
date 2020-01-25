import Data.Char
import Data.List (sort, elemIndex)
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

ampchain source [pa, pb, pc, pd, pe] = outputE
    where
        [outputE] = run source [pe, outputD]
        [outputD] = run source [pd, outputC]
        [outputC] = run source [pc, outputB]
        [outputB] = run source [pb, outputA]
        [outputA] = run source [pa, 0]

validphase phases =
    4 `elem` phases &&
    3 `elem` phases &&
    2 `elem` phases &&
    1 `elem` phases &&
    0 `elem` phases

answer1 = reverse $ sort $ map (\x -> (ampchain puzzle x)) phases
    where
        phases = filter (validphase)
            [[x,y,z,u,w] | x <- [0..4], y <- [0..4], z <- [0..4], u <- [0..4], w <- [0..4]]


runstep :: Int -> State -> (State, [Int], Bool)
runstep input (memory, pc) = ((memory', pc'), output, instruction == Halt)
    where
        instruction = parse memory pc

        memory' = execute instruction memory [input]
        pc' = (calcpc instruction) pc
        output = (calcOutput instruction) []
        input' = (calcInput instruction) [input]

runUntilOutput :: State -> Int -> (State, [Int], Bool)
runUntilOutput (memory, pc) input
    | halted = ((memory', pc'), output, halted)
    | length output > 0 = ((memory', pc'), output, halted)
    | otherwise = runUntilOutput (memory', pc') input
    where ((memory', pc'), output, halted) = runstep input (memory, pc)

foz [] = 0
foz (x:_) = x

amploop :: Int -> [State] -> Int -> Int
amploop input [stateA, stateB, stateC, stateD, stateE] maxPower = result
    where
        result = if (haltedE)
            then maxPower
            else (amploop (foz outputE) states' maxPower')
        maxPower' = max maxPower (foz outputE)
        states' = [stateA', stateB', stateC', stateD', stateE']
        (stateE', outputE, haltedE) = runUntilOutput stateE (head outputD)
        (stateD', outputD, _) = runUntilOutput stateD (head outputC)
        (stateC', outputC, _) = runUntilOutput stateC (head outputB)
        (stateB', outputB, _) = runUntilOutput stateB (head outputA)
        (stateA', outputA, _) = runUntilOutput stateA input

amploopStarter :: [Int] -> [Int] -> Int
amploopStarter source phases = amploop 0 states 0
    where
        states = map (\(state, _, _) -> state) $ map (\x -> runstep x (source, 0)) phases

validphase2 phases =
    5 `elem` phases &&
    6 `elem` phases &&
    7 `elem` phases &&
    8 `elem` phases &&
    9 `elem` phases

answer2 = reverse $ sort $ map ((amploopStarter puzzle)) phases
    where
        phases = filter (validphase2)
            [[x,y,z,u,w] | x <- [5..9], y <- [5..9], z <- [5..9], u <- [5..9], w <- [5..9]]

source :: [Int]
source = [1001,4,3,4,33]

echo :: [Int]
echo = [3, 0, 4, 0, 99]

puzzle :: [Int]
puzzle = [3,8,1001,8,10,8,105,1,0,0,21,38,59,76,89,106,187,268,349,430,99999,3,9,1002,9,3,9,101,2,9,9,1002,9,4,9,4,9,99,3,9,1001,9,5,9,1002,9,5,9,1001,9,2,9,1002,9,3,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,1001,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,5,9,4,9,99,3,9,1002,9,3,9,101,5,9,9,1002,9,3,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99]

test :: Int -> [Int]
test 0 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
test 1 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
test 2 = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
test 3 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
test 4 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
