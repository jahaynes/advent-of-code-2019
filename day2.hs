import           Control.Monad.ST                  (ST, runST)
import           Data.Vector.Unboxed.Mutable       (MVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Vector.Unboxed               (Vector)
import qualified Data.Vector.Unboxed         as V

newtype Ip = Ip Int

compute :: MVector s Int -> Ip -> ST s (Either String Int)
compute prog (Ip ip) = do

    op <- VM.read prog ip

    if op == 99
        then Right <$> VM.read prog 0
        else do

            [src_a, src_b, dst] <- mapM (VM.read prog) $ map (+ip) [1, 2, 3]

            case op of

                1 -> do
                    [a, b] <- mapM (VM.read prog) [src_a, src_b]
                    VM.write prog dst $! a + b
                    compute prog (Ip $ ip + 4)

                2 -> do
                    [a, b] <- mapM (VM.read prog) [src_a, src_b]
                    VM.write prog dst $! a * b
                    compute prog (Ip $ ip + 4)

                n  -> pure . Left $ "Unknown op code: " ++ show n

adjustAndRun :: Vector Int -> Int -> Int -> Either String Int
adjustAndRun input noun verb = runST $ do
    prog <- V.thaw input
    VM.write prog 1 noun
    VM.write prog 2 verb
    compute prog (Ip 0)

part1 :: Vector Int -> Either String Int
part1 input = adjustAndRun input 12 2

part2 :: Vector Int -> Either String Int
part2 input =
    let results = do
            noun <- [0..99]
            verb <- [0..99]
            case adjustAndRun input noun verb of
                Right 19690720 -> pure (noun, verb)
                _              -> []

    in case results of
        []    -> Left "No solution found"
        ((noun, verb) :_ ) -> Right $ 100 * noun + verb

main :: IO ()
main = do
    input <- V.fromList . (\input -> read ('[' : input ++ "]")) <$> readFile "./day2_input"
    print $ part1 input
    print $ part2 input
