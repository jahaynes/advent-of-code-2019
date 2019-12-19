import           Control.Parallel.Strategies
import           Data.Char        (isLower, toLower)
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
      $ [ "########################"
        , "#...............b.C.D.f#"
        , "#.######################"
        , "#.....@.a.B.c.d.A.e.F.g#"
        , "########################" ]

newSearch :: Coord -> Set Char -> Search
newSearch start gridKeys =
    Search { visited = S.singleton start
           , keys    = S.fromList "@."
           , allKeys = S.fromList "@." <> gridKeys
           , cost    = 0
           }

steps :: Grid Char -> Coord -> [Coord]
steps grid (Coord row col) =
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
        , haveKey || isKey ]

    where
    notVisited = not (S.member coord (visited search))

    haveKey = toLower (grid ! row ! col) `S.member` keys search

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

main :: IO ()
main = do

    -- input <- V.fromList . map V.fromList . lines <$> readFile "./day18_input"

    let start = findInGrid (=='@') input

    let search = newSearch start (getKeys input)

    print $ go search input $ start

go :: Search -> Grid Char -> Coord -> Int
go search grid coord@(Coord row col) = do

    let search' = updateKeys
                . updateVisited
                $ search

    if success search'
        then cost search'
        else case filter (isAccessible search' grid) . steps grid $ coord of
            [] -> 100000
            cs -> let search'' = search' {cost = cost search' + 1}
                  in minimum . parMap rpar (go search'' grid) $ cs

    where
    updateKeys search =
        let cell = grid ! row ! col
        in if isLower cell && not (cell `S.member` keys search)
               then search { keys    = S.insert cell (keys search)
                           , visited = S.singleton coord
                           }
               else search

    updateVisited search =
        search { visited = S.insert coord (visited search) }

    success search =
        keys search == allKeys search