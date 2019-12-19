import           Data.Char        (isLower)
import           Data.Set         (Set)
import qualified Data.Set    as S
import           Data.Vector      ((!), Vector)
import qualified Data.Vector as V

data Coord = Coord Int Int deriving (Eq, Ord, Show)

type Grid a = Vector (Vector a)

data Search =
    Search { visited :: Set Coord
           , keys    :: Set Char
           , allKeys :: Set Char
           , cost    :: Int
           } deriving Show

input :: Grid Char
input = V.fromList
      . map V.fromList
      $ [ "#########"
        , "#b.A.@.a#"
        , "#########" ]

newSearch :: Coord -> Set Char -> Search
newSearch start gridKeys =
    Search { visited = S.singleton start
           , keys    = S.fromList "@.#"
           , allKeys = S.fromList "@.#" <> gridKeys
           , cost    = 0
           }

steps :: Search -> Grid Char -> Coord -> [Coord]
steps search grid (Coord row col) =
    left ++ right ++ up ++ down
    where
    left  | col > 0   = [Coord row (col-1)]
          | otherwise = []
    right | col < V.length (grid ! 0) - 1 = [Coord row (col+1)]
          | otherwise = []
    up    | row > 0   = [Coord (row-1) col]
          | otherwise = []
    down  | row < V.length grid - 1 = [Coord (row+1) col]
          | otherwise = []

isAccessible :: Search -> Grid Char -> Coord -> Bool
isAccessible search grid coord@(Coord row col) =
    and [ notVisited
        , haveKey || isKey
        ]

    where
    notVisited = not (S.member coord (visited search))

    haveKey = grid ! row ! col `S.member` keys search

    isKey = isLower (grid ! row ! col)

getKeys :: Vector (Vector Char) -> Set Char
getKeys grid = S.fromList
             . filter isLower
             . concat
             . V.toList
             . V.map V.toList
             $ grid


coordinateGrid :: Vector (Vector a) -> Vector (Vector (Coord, a))
coordinateGrid =
    V.imap (\rowN row ->
        V.imap (\colN cell ->
            (Coord rowN colN, cell)) row)

findInGrid :: (a -> Bool) -> Grid a -> Coord
findInGrid p grid =
    let x = V.filter (not . null)
          . V.map (V.filter (\(_, a) -> p a))
          . coordinateGrid
          $ grid
    in fst $ x ! 0 ! 0

move :: Search -> Grid Char -> Coord -> Search
move search grid (Coord row col) = search

main :: IO ()
main = do

    let start = findInGrid (=='@') input

    let search = newSearch start (getKeys input)

    --print $ filter (isAccessible search input)
    --      . steps search input
    --      $ start

    go search input start

go search grid start = do

    print search