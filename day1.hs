data Recursive = Simple | Recursive

class CalcFuel a where
  calcFuel :: Recursive -> a -> Fuel

class CalcMass a where
  calcMass :: a -> Mass

newtype Mass =
  Mass Integer deriving (Eq, Ord)

instance CalcFuel Mass where

  calcFuel Simple (Mass x) =
    max (Fuel 0) (Fuel ((x `div` 3) - 2))

  calcFuel Recursive mass
    | mass <= Mass 0 = Fuel 0
    | otherwise =
        let newFuel = calcFuel Simple mass
            newMass = calcMass newFuel
        in newFuel <> calcFuel Recursive newMass

newtype Fuel =
  Fuel Integer deriving (Eq, Ord, Show)

instance Semigroup Fuel where
  Fuel a <> Fuel b = Fuel (a + b)

instance Monoid Fuel where
  mempty = Fuel 0

instance Semigroup Mass where
  Mass a <> Mass b = Mass (a + b)

instance Monoid Mass where
  mempty = Mass 0

instance CalcMass Fuel where
  calcMass (Fuel a) = Mass a

readMasses :: IO [Mass]
readMasses = map (Mass . read) . lines <$> readFile "./day1_input"

part1 :: [Mass] -> Fuel
part1 = mconcat . map (calcFuel Simple)

part2 :: [Mass] -> Fuel
part2 = mconcat . map (calcFuel Recursive)

main :: IO ()
main = do
  masses <- readMasses
  print (part1 masses)
  print (part2 masses)

