import           Control.Monad.ST                  (ST, runST)
import           Data.Vector.Unboxed.Mutable       (MVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Vector.Unboxed               (Vector)
import qualified Data.Vector.Unboxed         as V

newtype Ip = Ip Int

data Mode = Position | Immediate

fromDigit :: Char -> Mode
fromDigit '0' = Position
fromDigit '1' = Immediate
fromDigit _   = error "Unknown mode"

compute :: ST s Int -> [Int] -> MVector s Int -> Ip -> ST s (Either String [Int])
compute getInput out prog (Ip ip) = do

    (op, modeA, modeB, modeD) <- readOp prog (Ip ip)

    case op of

        1 -> do
            [src_a, src_b, dst] <- mapM (VM.read prog) $ map (+ip) [1, 2, 3]

            a <- case modeA of
                     Position -> VM.read prog src_a
                     Immediate -> pure src_a

            b <- case modeB of
                     Position -> VM.read prog src_b
                     Immediate -> pure src_b

            case modeD of
                Position -> VM.write prog dst $! a + b
                Immediate -> error "Immediate dest!"

            compute getInput out prog (Ip $ ip + 4)

        2 -> do
            [src_a, src_b, dst] <- mapM (VM.read prog) $ map (+ip) [1, 2, 3]

            a <- case modeA of
                        Position -> VM.read prog src_a
                        Immediate -> pure src_a

            b <- case modeB of
                        Position -> VM.read prog src_b
                        Immediate -> pure src_b

            case modeD of
                Position -> VM.write prog dst $! a * b
                Immediate -> error "Immediate dest!"

            compute getInput out prog (Ip $ ip + 4)

        3 -> do
            inp <- getInput
            dst <- VM.read prog (1+ip)

            case modeD of
                Position -> VM.write prog dst inp
                Immediate -> error "Immediate dest!"

            compute getInput out prog (Ip $ ip + 2)

        4 -> do
            src_a <- VM.read prog (1+ip)
            a <- case modeA of
                     Position -> VM.read prog src_a
                     Immediate -> pure src_a

            compute getInput (out ++ [a]) prog (Ip $ ip + 2)

        5 -> do
            [src_a, src_b] <- mapM (VM.read prog) $ map (+ip) [1, 2]
            a <- case modeA of
                     Position -> VM.read prog src_a
                     Immediate -> pure src_a
            b <- case modeB of
                     Position -> VM.read prog src_b
                     Immediate -> pure src_b
            let ip' = Ip (if a /= 0 then b else ip + 3)
            compute getInput out prog ip'

        6 -> do
            [src_a, src_b] <- mapM (VM.read prog) $ map (+ip) [1, 2]
            a <- case modeA of
                     Position -> VM.read prog src_a
                     Immediate -> pure src_a
            b <- case modeB of
                     Position -> VM.read prog src_b
                     Immediate -> pure src_b
            let ip' = Ip (if a == 0 then b else ip + 3)
            compute getInput out prog ip'

        7 -> do
            [src_a, src_b, dst] <- mapM (VM.read prog) $ map (+ip) [1, 2, 3]

            a <- case modeA of
                     Position -> VM.read prog src_a
                     Immediate -> pure src_a

            b <- case modeB of
                     Position -> VM.read prog src_b
                     Immediate -> pure src_b

            let lt = if a < b then 1 else 0

            case modeD of
                Position -> VM.write prog dst $! lt
                Immediate -> error "Immediate dest!"

            compute getInput out prog (Ip $ ip + 4)

        8 -> do
            [src_a, src_b, dst] <- mapM (VM.read prog) $ map (+ip) [1, 2, 3]

            a <- case modeA of
                        Position -> VM.read prog src_a
                        Immediate -> pure src_a

            b <- case modeB of
                        Position -> VM.read prog src_b
                        Immediate -> pure src_b

            let lt = if a == b then 1 else 0

            case modeD of
                Position -> VM.write prog dst $! lt
                Immediate -> error "Immediate dest!"

            compute getInput out prog (Ip $ ip + 4)

        99 -> pure $ Right out

        n  -> pure . Left $ "Unknown op code " ++ show n ++ " at " ++ show ip

readOp :: MVector s Int -> Ip -> ST s (Int, Mode, Mode, Mode)
readOp prog (Ip ip) = do
    rawOp <- VM.read prog ip

    let (modes, op) = rawOp `divMod` 100

    case modes of
        0 -> pure (op, Position, Position, Position)
        _ -> case reverse . show $ modes of
                [a, b, d] -> pure (op, fromDigit a, fromDigit b, fromDigit d)
                [a, b]    -> pure (op, fromDigit a, fromDigit b, Position)
                [a]       -> pure (op, fromDigit a, Position, Position)
                _         -> error $ show (rawOp, modes, op)

part1 :: Vector Int -> ST s (Either String [Int])
part1 input = do
    prog <- V.thaw input
    compute (pure 1) [] prog (Ip 0)

part2 :: Vector Int -> ST s (Either String [Int])
part2 input = do
    prog <- V.thaw input
    compute (pure 5) [] prog (Ip 0)

main :: IO ()
main = do
    input <- V.fromList . (\input -> read ('[' : input ++ "]")) <$> readFile "./day5_input"
    print $ runST (part1 input)
    print $ runST (part2 input)
