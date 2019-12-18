import           Data.Vector      ((!), Vector)
import qualified Data.Vector as V

data Coord = Coord Int Int deriving Show

type Grid a = Vector (Vector a)

input :: Grid Char
input = V.fromList
      . map V.fromList
      $ [ "#########"
        , "#b.A.@.a#"
        , "#########" ]

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