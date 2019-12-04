import Data.Either (isRight)
import Data.List   (group, sort)

sixDigit :: Int -> Either String ()
sixDigit = range 100000 999999
    where
    range :: Int -> Int -> Int -> Either String ()
    range mi mx n | n < mi = Left "Too low"
                  | n > mx = Left "Too high"
                  | otherwise = pure ()

adjacentPair :: Int -> Either String ()
adjacentPair n =
    case filter (>1) . map length . group . show $ n of
        [] -> Left "No adjacent pairs"
        _  -> pure ()

adjacentStrictPair :: Int -> Either String ()
adjacentStrictPair n =
    case filter (==2) . map length . group . show $ n of
        [] -> Left "No adjacent pairs"
        _  -> pure ()

neverDecrease :: Int -> Either String ()
neverDecrease n =
    let s = show n in
    if s == sort s
        then pure ()
        else Left "Decreasing pair"

part1 :: Int -> Int -> Int
part1 lo hi = length
            . filter isRight
            . map check
            $ [lo..hi]
    where
    check n = do
        sixDigit n
        adjacentPair n
        neverDecrease n

part2 :: Int -> Int -> Int
part2 lo hi = length
            . filter isRight
            . map check
            $ [lo..hi]
    where
    check n = do
        sixDigit n
        adjacentStrictPair n
        neverDecrease n

main :: IO ()
main = do
    let (lo, hi) = (246540, 787419)
    print (part1 lo hi)
    print (part2 lo hi)