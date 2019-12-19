import           Data.Set         (Set)
import qualified Data.Set    as S
import           Data.Vector      ((!), Vector)
import qualified Data.Vector as V

data Coord = Coord Int Int deriving Show

type Grid a = Vector (Vector a)

data Search =
    Search { visited :: Set Coord
           , keys    :: Set Char
           }

input :: Grid Char
input = V.fromList
      . map V.fromList
      $ [ "#########"
        , "#b.A.@.a#"
        , "#########" ]

steps :: Search -> Grid Char -> Coord -> [Coord]
steps = undefined

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
main = print $ findInGrid (=='@') input