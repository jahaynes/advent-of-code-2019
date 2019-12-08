{-# LANGUAGE DeriveFunctor #-}

import Data.Char       (isDigit)
import Data.List.Split (chunksOf)
import Data.List       (minimumBy)

newtype Layer a = Layer [Row a]
    deriving Functor

newtype Row a = Row [a]
    deriving Functor

newtype Width = Width Int

newtype Height = Height Int

class NumOfDigit a where
    numOfDigit :: Int -> a -> Int

instance NumOfDigit a => NumOfDigit (Layer a) where
    numOfDigit d (Layer rs) = sum . map (numOfDigit d) $ rs

instance NumOfDigit a => NumOfDigit (Row a) where
    numOfDigit d (Row xs) = sum . map (numOfDigit d) $ xs

instance NumOfDigit Int where
    numOfDigit d i | d == i    = 1
                   | otherwise = 0

class Overlay a where
    overlay :: a -> a -> a

instance Overlay a => Overlay (Layer a) where
    overlay (Layer x) (Layer y) = Layer (zipWith overlay x y)

instance Overlay a => Overlay (Row a) where
    overlay (Row x) (Row y) = Row (zipWith overlay x y)

instance Overlay Int where
    overlay 2 y = y
    overlay x _ = x

part1 :: [Layer Int] -> Int
part1 parsed =
    let fewest0Layer = minimumBy (\l1 l2 -> compare (numOfDigit 0 l1) (numOfDigit 0 l2)) parsed
    in numOfDigit 1 fewest0Layer * numOfDigit 2 fewest0Layer

part2 :: [Layer Int] -> String
part2 = render . foldr1 overlay

    where
    render :: Layer Int -> String
    render (Layer rows) = unlines $ map (\(Row is) -> map i2c is) rows
        where
        i2c 0 = ' '
        i2c 1 = '*'
        i2c _ = undefined

main :: IO ()
main = do
    input <- map (read . (:[]))
           . filter isDigit
        <$> readFile "./day8_input"
    let parsed = parse (Width 25) (Height 6) input
    print $ part1 parsed
    putStrLn $ part2 parsed

parse :: Width -> Height -> [a] -> [Layer a]
parse (Width w) (Height h) input =
    let len = length input
        numLayers = len `div` (w * h)
        layerSize = len `div` numLayers
        layers    = chunksOf layerSize input
    in map (Layer . map Row . chunksOf w) layers
