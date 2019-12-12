import           Data.List        (groupBy, sort, sortOn, partition)
import qualified Data.Set    as S
import           Data.Vector      ((!), Vector)
import qualified Data.Vector as V

type Rise = Int
type Run = Int
type Angle = Float
type RelX = Int
type RelY = Int

data Trace = Trace Rise Run
    deriving (Eq, Ord)

data Asteroid = Traced Trace RelX RelY
                | Angled Angle Trace RelX RelY
                | Absolute Int Int
                    deriving (Eq, Ord)

allAsteroidsRelative :: Int -> Int -> Vector (Vector Char) -> [(Int, Int)]
allAsteroidsRelative x y grid = do
    let height = V.length  grid
        width  = V.length (grid ! 0)
    j <- [0 .. height - 1]
    i <- [0 .. width  - 1]
    if grid ! j ! i == '.'
        then []
        else [(i - x, j - y)]

trace :: (Rise, Run) -> Trace
trace (rise, run) =
    Trace (rise `div` d) (run `div` d)
        where d = gcd rise run

part1 :: Vector (Vector Char) -> (Int, Int, Int)
part1 grid =
    let allAsteroids = allAsteroidsRelative 0 0 grid
        counts = map (\(x, y) -> ( S.size
                                 . S.fromList
                                 . map trace
                                 . filter (/= (0,0))
                                 $ allAsteroidsRelative x y grid, x, y)
                                 ) allAsteroids
    in maximum counts

toAsteroid :: (RelX, RelX) -> Asteroid
toAsteroid (relX, relY) = Traced (trace (relY, relX)) relX relY

toAngled :: Asteroid -> Asteroid
toAngled (Traced tr relX relY) = Angled (getAngle tr) tr relX relY

trav :: [[Asteroid]] -> [Asteroid]
trav = go []
  where
  go acc       [] = reverse acc
  go acc ([]:grs) = go acc grs
  go acc ((a:as):grs) = go (a:acc) (grs ++ [as])

sortAngles :: [Asteroid] -> [Asteroid]
sortAngles xs =
    let sorted     = sort xs
        (pos, neg) = partition (\(Angled angle _ _ _) -> angle >= 0) sorted
    in pos ++ neg

getAngle :: Trace -> Float
getAngle (Trace rise run) =
    let ccXradians = atan2 (fromIntegral (-rise)) (fromIntegral run)
        ccXdegrees = ccXradians * 180 / pi
        degrees    = 90.0 - ccXdegrees
    in degrees

toAbs :: Int -> Int -> Asteroid -> Asteroid
toAbs oX oY (Angled _ _ relX relY) = Absolute (relX + oX) (relY + oY)

sqDist :: Asteroid -> Int
sqDist (Angled _ _ relX relY) = relX * relX + relY * relY

part2 :: Vector (Vector Char) -> Int -> Int -> Int
part2 grid x0 y0 = do

    let otherAsteroids = filter (/= (0,0)) 
                       . allAsteroidsRelative x0 y0
                       $ grid

    let traced = map toAsteroid otherAsteroids

    let depthAngled = map toAngled traced

    let properOrder = zip [1..]
                    . map (toAbs x0 y0)
                    . trav
                    . map (sortOn sqDist)
                    . groupBy (\(Angled _ tr1 _ _) (Angled _ tr2 _ _) -> tr1 == tr2) 
                    . sortAngles
                    $ depthAngled

    let (_, Absolute absx absy) = properOrder !! 199
    (100 * absx + absy)

main :: IO ()
main = do

    input <- lines <$> readFile "./day10_input"

    let rows = map V.fromList input
    let grid = V.fromList rows

    let one@(_, x, y) = part1 grid
    print one

    print $ part2 grid x y
