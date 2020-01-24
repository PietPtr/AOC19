import Data.Char

type Left = Int
type Right = Int
type Address = Int
type Value = Int

type Memory = [Int]
type PC = Int

data Moded =
      Immediate Int
    | Position Int
    deriving (Show)

data Instruction =
      Multiply Moded Moded Moded
    | Add Moded Moded Moded
    | Input Moded
    | Output Moded
    | Halt
    deriving (Show)

testdec :: Int -> Int -> Int
testdec value position = ord ((padding ++ str) !! position) - 48
    where
        padding = take (5 - length str) $ repeat '0'
        str = show value

parse :: Memory -> PC -> Instruction
parse memory pc = case (opcode) of
    1 -> (Multiply (makeValue 0) (makeValue 1) (makeValue 2))
    2 -> (Add (makeValue 0) (makeValue 1) (makeValue 2))
    3 -> (Input (makeValue 0))
    4 -> (Output (makeValue 0))
    99 -> (Halt)
    where
        opcode = (testdec decimalInstruction 3) * 10 + (testdec decimalInstruction 4)
        decimalInstruction = memory !! pc

        makeValue index = getModed (modes !! index) (args !! index)

        getModed 0 value = (Position value)
        getModed 1 value = (Immediate value)

        args = drop (pc+1) memory
        modes = map (\x -> testdec decimalInstruction x) [2,1,0]


operate :: Memory -> Moded -> (Int -> Int -> Int) -> Moded -> Moded -> Memory
operate memory left op right destination = memory
    where
        (Immediate lval) = left
        (Immediate rval) = right

-- execute :: Instruction -> Memory -> (Memory, PC)
execute (Multiply left right destination) memory = operate memory left (*) right destination
