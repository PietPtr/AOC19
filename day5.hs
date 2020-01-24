import Debug.Trace

source :: [Int]
-- source = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,13,19,23,2,23,9,27,1,6,27,31,2,10,31,35,1,6,35,39,2,9,39,43,1,5,43,47,2,47,13,51,2,51,10,55,1,55,5,59,1,59,9,63,1,63,9,67,2,6,67,71,1,5,71,75,1,75,6,79,1,6,79,83,1,83,9,87,2,87,10,91,2,91,10,95,1,95,5,99,1,99,13,103,2,103,9,107,1,6,107,111,1,111,5,115,1,115,2,119,1,5,119,0,99,2,0,14,0]
-- source = [1,3,2,3]
source = [1,9,10,3,2,3,11,0,99,30,40,50]

operate :: (Int -> Int -> Int) -> [Int] -> Int -> [Int]
operate op memory pc = newmem
    where
        newmem = before ++ [newValue] ++ after
        (before,_:after) = splitAt dest memory
        newValue = (memory !! left) `op` (memory !! right)
        left = memory !! (pc + 1)
        right = memory !! (pc + 2)
        dest = memory !! (pc + 3)

step :: [Int] -> Int -> [Int]
step memory pc
    | ins == 1 = step (operate (+) memory pc) (pc+4)
    | ins == 2 = step (operate (*) memory pc) (pc+4)
    | ins == 99 = memory
    where
        ins = memory !! pc

start = step source 0
