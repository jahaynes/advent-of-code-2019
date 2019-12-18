import Data.Char (isDigit)
import Data.List (groupBy)

data Moon = Moon { x  :: Int
                 , y  :: Int
                 , z  :: Int
                 , vx :: Int
                 , vy :: Int
                 , vz :: Int
                 } deriving Show

new_moon :: Int -> Int -> Int -> Moon
new_moon x y z = Moon x y z 0 0 0

steps :: Int -> [Moon] -> [Moon]
steps 0 ms = ms
steps n ms = steps (n-1) (step ms)

step :: [Moon] -> [Moon]
step = map velo . applyGrav

    where
    applyGrav ms = map (\m -> foldr grav m ms) ms

    grav :: Moon -> Moon -> Moon
    grav other = gravX . gravY . gravZ

        where
        gravX :: Moon -> Moon
        gravX moon =
            case compare (x moon) (x other) of
                LT -> moon { vx = vx moon + 1}
                EQ -> moon
                GT -> moon { vx = vx moon - 1}

        gravY :: Moon -> Moon
        gravY moon =
            case compare (y moon) (y other) of
                LT -> moon { vy = vy moon + 1}
                EQ -> moon
                GT -> moon { vy = vy moon - 1}

        gravZ :: Moon -> Moon
        gravZ moon =
            case compare (z moon) (z other) of
                LT -> moon { vz = vz moon + 1}
                EQ -> moon
                GT -> moon { vz = vz moon - 1}

    velo :: Moon -> Moon
    velo m = m { x = x m + vx m
               , y = y m + vy m
               , z = z m + vz m
               }

energy :: Moon -> Int
energy (Moon x y z vx vy vz) = (abs x  + abs y  + abs z)
                             * (abs vx + abs vy + abs vz)

parse :: FilePath -> IO [Moon]
parse file = do
    ls <- lines <$> readFile file
    pure . map (\[a,b,c] -> new_moon a b c)
         . map (map read)
         . map parse'
         $ ls
    where
    parse' = filter (numeric . head)
           . groupBy (\a b -> numeric a == numeric b)

    numeric '-' = True
    numeric c | isDigit c = True
              | otherwise = False

part1 = sum . map energy . steps 1000

states ms f = go 0 ms
    where
    go 0 acc = go 1 (step acc)
    go n acc | map f acc == map f ms = n
             | otherwise             = go (n+1) (step acc)




main :: IO ()
main = do

    moons <- parse "day12_input"

    print . part1 $ moons

    print . foldl1 lcm . map (states moons) $ [x, y, z, vx, vy, vz]


    -- 564767435644116 -- too high