import Debug.Trace

data Tile = Full | Empty deriving (Show, Eq)
data SpaceObject = Asteroid Coord deriving (Show, Eq)
type Space = [[Tile]]
type Coord = (Int, Int)

at :: Coord -> Space -> Tile
at (x, y) space = space !! y !! x

isIn :: Coord -> Space -> Bool
isIn (x, y) space = (y < (length space)) && (x < (length (space !! 0))) && (x >= 0) && (y >= 0)

x = fst
y = snd

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

parse :: String -> Space
parse input = parseLines (take size input) (drop size input)
    where
        size = isqrt (length input)
        parseLines line [] = [parseLine line]
        parseLines line allInput = (parseLine line) : (parseLines (take size allInput) (drop size allInput))

        parseLine [] = []
        parseLine ('.':line) = Empty : (parseLine line)
        parseLine ('#':line) = Full : (parseLine line)

asteroids :: Space -> [SpaceObject]
asteroids space = foldl folder [] coords
    where
        folder list coord = list ++ (if (tile == Full) then [(Asteroid coord)] else [])
            where tile = at coord space
        coords = [(x, y) | y <- [0..(size-1)], x <- [0..(size-1)]]
        size = length (space !! 0)

countFromDirection :: Coord -> Coord -> Int -> Space -> Int
countFromDirection pos (dx, dy) step space = hoeveelUnitsZijnEr + (trace ("ok") enVerderDan)
    where
        hoeveelUnitsZijnEr = if (tile == Full) then 1 else 0
        enVerderDan = if (nextPosition `isIn` space)
            then countFromDirection pos (dx, dy) (step + 1) space
            else 0
        tile = at (px, py) space
        (px, py) = (x pos + step * dx, y pos + step * dy)
        nextPosition = (px + dx, py + dy)

checkLine :: SpaceObject -> SpaceObject -> Space -> Int
checkLine (Asteroid home) (Asteroid roid) space = countFromDirection roid reduced 1 space
    where
        reduced = (vx `div` vectorGcd, vy `div` vectorGcd)
        vectorGcd = trace (show vectorGcd) (gcd vx vy)
        (vx, vy) = (x roid - x home, y roid - y roid)

countDetectable :: SpaceObject -> Space -> [SpaceObject] -> Int
countDetectable _ _ [] = 0
countDetectable (Asteroid homeCoords) space ((Asteroid targetCoords):sos) = 0

trapaan = countDetectable (roids !! 0) space roids
    where
        roids = asteroids space
        space = parse $ test 0

asteroidsT = (asteroids . parse . test)
spaceT = (parse . test)
home x = (!! 0) $ asteroidsT x

test :: Int -> String
test 0 = ".#..#.....#####....#...##"
test 1 = "......#.#.#..#.#......#######..#.#.###...#..#.......#....#.##..#....#..##.#..#####...#..#..#....####"
test 2 = "#.#...#.#..###....#..#....#...##.#.#.#.#....#.#.#..##..###.#..#...##....##....##......#....####.###."
test 3 = ".#..#..#######.###.#....###.#...###.##.###.##.#.#.....###..#..#.#..#.##..#.#.###.##...##.#.....#.#.."
test 4 = ".#..##.###...#########.############..##..#.######.########.#.###.#######.####.#.#####.##.#.##.###.##..#####..#.##############################.####....###.#.#.####.######################.##.###..####....######..##.###########.##.####...##..#.#####..#.######.#####...#.##########...#.##########.#######.####.#.###.###.#.##....##.##.###..#####.#.#.###########.####.#.#.#####.####.######.##.####.##.#..##"

puzzle :: String
puzzle = ".###..#######..####..##...#########.#.###...###.#....####..#...#######...#..####..##.#.....#....##.#.#.....####.#######.###..##......#.#..###..###.##.#.#####....##.##..###....#####...##.##.####.##..#...#####.#..###.##..#....####.####.###.#.####..#..#....###...#####..#..##...####.######....#.####.####.##...###.####..##....##.#..#.###.#.##.####..#...#..##..##....#.#..##..#.#..###.##.#..######.#..#..####.#.....#####.##........########.#.#######..#.#.##..#..####...#..#.#..##.##..#####..##.#..#...#####.###.##.##....#.#.######.#####.#.####.#..##..###...###.#.#..#.#.#.#..#.#......#.###...###..##.##.#.#..#.#......#..#..##.##.##.##.#...##.##.##.#..##.###.#.#...##..#####.###.##.####.#..#.#.##.######.#...#.#####.##...#...#.##...#."
