
import           Control.Monad.ST              (ST, runST)
import qualified Data.HashTable.ST.Cuckoo as H
import           Data.List                     (minimumBy)
import           Data.List.Split               (splitOn)
import qualified Data.Set                 as S
import           Data.Set         (Set)

newtype Id = Id Int
    deriving (Eq, Ord)

data Dir = U | L | D | R
    deriving Read

data Segment = Segment Id Dir Int

data Join = Join (Int, Int) (Set (Id, Clock))

newtype Clock = Clock { unclock :: Int }
    deriving (Eq, Ord)

parseInput :: String -> [[Segment]]
parseInput input = zipWith parseWire [1..] (lines input)
    where
    parseWire i = map ((\(d, n) -> Segment (Id i) d n) . parseSeg) . splitOn ","
        where
        parseSeg :: String -> (Dir, Int)
        parseSeg     [] = error "Not a segment"
        parseSeg (c:cs) = (read [c], read cs)

runTable :: [[Segment]] -> ST s [Join]
runTable wires = do

    grid <- H.new
    mapM_ (go (Clock 0) (0, 0) grid) wires
    joins <- H.foldM (\joins (c, ids) -> pure $ if S.size ids > 1 then Join c ids:joins else joins) [] grid
    pure . filter (\(Join c   _) -> c /= (0, 0))
         . filter (\(Join _ sic) -> S.size (S.map fst sic) > 1)
         $ joins

    where
    go                _        _    _                   [] = pure ()
    go cl@(Clock clock) c@(x, y) grid (Segment i d n:segs) = do
        mark 
        if n == 0
            then go cl c grid segs
            else let c' = case d of
                            U -> (x, y - 1)
                            L -> (x - 1, y)
                            D -> (x, y + 1)
                            R -> (x + 1, y)
                     n' = n - 1
                 in go (Clock $ clock + 1) c' grid (Segment i d n':segs)

        where
        mark = H.mutate grid c $ \mv ->
            case mv of
                Nothing -> (Just (S.singleton (i, cl)), ())
                Just is -> (Just (S.insert (i, cl) is), ())

part1 :: [Join] -> Int
part1 = minimum
      . filter (/= 0)
      . map byManhattan

    where
    byManhattan :: Join -> Int
    byManhattan (Join (x, y) _) = abs x + abs y

part2 :: [Join] -> Int
part2 = sumClocks
      . minimumBy (\j1 j2 -> compare (sumClocks j1) (sumClocks j2))

    where
    sumClocks :: Join -> Int
    sumClocks (Join _ idsClocks) = sum 
                                 . map (unclock . snd)
                                 . S.toList
                                 $ idsClocks

main :: IO ()
main = do
    rawInput <- readFile "./day3_input"
    let wires = parseInput rawInput
    let joins = runST (runTable wires)
    print (part1 joins)
    print (part2 joins)
