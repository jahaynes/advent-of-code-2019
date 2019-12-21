import AStar

import Debug.Trace
import           Control.Monad.Identity (Identity)
import           Data.Char              (isLower, toLower)
import           Data.Maybe             (catMaybes)
import           Data.Set               (Set)
import qualified Data.Set    as S
import           Data.Vector            ((!), Vector)
import qualified Data.Vector as V

data Coord = Coord Int Int deriving (Eq, Ord, Show)

type Grid a = Vector (Vector a)

data Cost = Cost Int
          | Forbidden
              deriving (Eq, Ord, Show)

instance Semigroup Cost where
    _ <> Forbidden   = Forbidden
    Forbidden <> _   = Forbidden
    Cost a <> Cost b = Cost (a + b)

instance Monoid Cost where
    mempty = Cost 0

input :: Grid Char
input = V.fromList
      . map V.fromList
      $ [ "########################"
        , "#@..............ac.GI.b#"
        , "###d#e#f################"
        , "###A#B#C################"
        , "###g#h#i################"
        , "########################" ]


heur :: Coord -> Goal Coord -> Cost
heur (Coord x1 y1) (Goal (Coord x2 y2)) =
    Cost $ abs (x2 - x1) + abs (y2 - y1)

solver :: KeySet -> Grid Char -> Start Coord -> Goal Coord -> Solver Identity Coord Cost
solver keyset grid start goal =
    Solver { getStart  = start
           , getGoal   = goal
           , heuristic = heur
           , expand    = expander keyset grid
           }

newtype KeySet =
    KeySet (Set Char)

expander :: KeySet -> Grid Char -> Cost -> Coord -> Identity [(Coord, Cost)]
expander (KeySet ks) grid cost (Coord r c) =

    pure . map (\co -> (co, cost <> Cost 1))
         . filter (\co -> iskey co || haveKey co)
         . filter inBounds
         $ [ Coord (r + 1) c
           , Coord (r - 1) c
           , Coord r (c + 1)
           , Coord r (c - 1)
           ]

    where
    inBounds (Coord row col) =
        row < V.length grid && 
        col < V.length (grid ! row)

    iskey (Coord row col) =
        isLower (grid ! row ! col)

    haveKey (Coord row col) =
        toLower (grid ! row ! col) `S.member` ks

go :: Cost -> Grid Char -> Start Coord -> KeySet -> [(Char, Coord)] -> Identity Cost
go Forbidden _     _                _  _ = pure Forbidden
go cost      _     _                _ [] = trace (show cost) $ pure cost
go cost   grid start keys@(KeySet ks) xs = do

    paths <- catMaybes <$> mapM (\(x, xc) -> do       
        mPath <- astar (solver keys grid start (Goal xc))
        pure $ case mPath of
                   Nothing   -> Nothing
                   Just path -> Just (x, last path, Cost $ length path - 1)
        ) xs

    soln <- mapM (\(c, sc, cost') -> do
        let xs'   = filter (\(x,_) -> x /= c) xs
            keys' = KeySet (S.insert c ks)
        go (cost <> cost') grid (Start sc) keys' xs'
        ) paths

    pure . minimum $ soln

main :: IO ()
main = do
    -- input <- V.fromList . map V.fromList . lines <$> readFile "./day18_input"
    let start = Start $ findInGrid (=='@') input
        keys = getKeys input
        keyCoords = map (\k -> findInGrid (==k) input) keys 
    print $ go mempty input start (KeySet (S.fromList "@.")) (zip keys keyCoords)

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

getKeys :: Vector (Vector Char) -> [Char]
getKeys grid = filter isLower
             . concat
             . V.toList
             . V.map V.toList
             $ grid
