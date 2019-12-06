
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map as M

loadOrbits :: String -> Map String String
loadOrbits = M.fromList
           . map ((\ [a, b] -> (b, a)) . splitOn ")")
           . lines 

main :: IO ()
main = do
    orbits <- loadOrbits <$> readFile "./day6_input"
    print $ part1 orbits
    print $ part2 orbits

part1 :: Map String String -> Int
part1 orbits = sum
             . map (trace (\_ acc -> 1 + acc) 0 orbits)
             . filter (/= "COM")
             . M.keys
             $ orbits

part2 :: Map String String -> Int
part2 orbits =
    let youTrace = trace (:) [] orbits "YOU"
        sanTrace = trace (:) [] orbits "SAN"
        common = length . takeWhile (uncurry (==)) . zip youTrace $ sanTrace
    in length youTrace
     + length sanTrace
     - 2 * common

trace :: (String -> a -> a) -> a -> Map String String -> String -> a
trace f acc orbits x =
    case M.lookup x orbits of
        Just "COM" -> f "COM" acc
        Just o     -> trace f (f o acc) orbits o
        Nothing    -> undefined