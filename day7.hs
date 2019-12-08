import           Control.Monad                     (replicateM, unless)
import           Control.Monad.ST                  (ST, runST)
import           Data.List                         (permutations)
import           Data.Maybe                        (fromMaybe)
import           Data.STRef
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

data Computer s =
    Computer { isHalted :: STRef s Bool
             , getMem   :: MVector s Int
             , getIp    :: STRef s Ip
             }

newtype InputMaskin s a =
    InputMaskin (ST s a)

newtype OutputMaskin s a =
    OutputMaskin (a -> ST s ())

step :: Computer s
     -> InputMaskin s Int
     -> OutputMaskin s Int
     -> ST s ()
step computer (InputMaskin readInput) (OutputMaskin writeOutput) = do

    let mem = getMem computer

    Ip ip <- readSTRef (getIp computer)
    (op, modeA, modeB, modeD) <- readOp mem (Ip ip)

    case op of
        1 -> do
            [src_a, src_b, dst] <- mapM (VM.read mem) $ map (+ip) [1, 2, 3]
            a <- case modeA of
                     Position -> VM.read mem src_a
                     Immediate -> pure src_a
            b <- case modeB of
                     Position -> VM.read mem src_b
                     Immediate -> pure src_b
            case modeD of
                Position -> VM.write mem dst $! a + b
                Immediate -> error "Immediate dest!"
            writeSTRef (getIp computer) (Ip $ ip + 4)

        2 -> do
            [src_a, src_b, dst] <- mapM (VM.read mem) $ map (+ip) [1, 2, 3]
            a <- case modeA of
                     Position -> VM.read mem src_a
                     Immediate -> pure src_a
            b <- case modeB of
                     Position -> VM.read mem src_b
                     Immediate -> pure src_b
            case modeD of
                Position -> VM.write mem dst $! a * b
                Immediate -> error "Immediate dest!"

            writeSTRef (getIp computer) (Ip $ ip + 4)

        3 -> do
            inp <- readInput
            dst <- VM.read mem (1+ip)
            case modeD of
                Position -> VM.write mem dst inp
                Immediate -> error "Immediate dest!"
            writeSTRef (getIp computer) (Ip $ ip + 2)

        4 -> do
            src_a <- VM.read mem (1+ip)
            a <- case modeA of
                     Position -> VM.read mem src_a
                     Immediate -> pure src_a
            writeOutput a
            writeSTRef (getIp computer) (Ip $ ip + 2)

        5 -> do
            [src_a, src_b] <- mapM (VM.read mem) $ map (+ip) [1, 2]
            a <- case modeA of
                     Position -> VM.read mem src_a
                     Immediate -> pure src_a
            b <- case modeB of
                     Position -> VM.read mem src_b
                     Immediate -> pure src_b
            writeSTRef (getIp computer) (Ip $ if a /= 0 then b else ip + 3)

        6 -> do
            [src_a, src_b] <- mapM (VM.read mem) $ map (+ip) [1, 2]
            a <- case modeA of
                     Position -> VM.read mem src_a
                     Immediate -> pure src_a
            b <- case modeB of
                     Position -> VM.read mem src_b
                     Immediate -> pure src_b
            writeSTRef (getIp computer) (Ip $ if a == 0 then b else ip + 3)

        7 -> do
            [src_a, src_b, dst] <- mapM (VM.read mem) $ map (+ip) [1, 2, 3]
            a <- case modeA of
                        Position -> VM.read mem src_a
                        Immediate -> pure src_a
            b <- case modeB of
                        Position -> VM.read mem src_b
                        Immediate -> pure src_b
            let lt = if a < b then 1 else 0
            case modeD of
                Position -> VM.write mem dst $! lt
                Immediate -> error "Immediate dest!"
            writeSTRef (getIp computer) (Ip $ ip + 4)

        8 -> do
            [src_a, src_b, dst] <- mapM (VM.read mem) $ map (+ip) [1, 2, 3]
            a <- case modeA of
                        Position -> VM.read mem src_a
                        Immediate -> pure src_a
            b <- case modeB of
                        Position -> VM.read mem src_b
                        Immediate -> pure src_b
            let lt = if a == b then 1 else 0
            case modeD of
                Position -> VM.write mem dst $! lt
                Immediate -> error "Immediate dest!"
            writeSTRef (getIp computer) (Ip $ ip + 4)

        99 -> writeSTRef (isHalted computer) True

newtype InputQueue s a =
    InputQueue (STRef s [a])

queueFromList :: [a] -> ST s (InputQueue s a)
queueFromList xs = InputQueue <$> newSTRef xs

enqueue :: InputQueue s a -> a -> ST s ()
enqueue (InputQueue stref) x = modifySTRef' stref (\q -> q ++ [x])

dequeue :: InputQueue s a -> ST s a
dequeue (InputQueue stref) = do
    q <- readSTRef stref
    case q of
        [] -> error "empty"
        (x:xs) -> do
            writeSTRef stref xs
            pure x

asInputMaskin :: InputQueue s a -> InputMaskin s a
asInputMaskin q = InputMaskin (dequeue q)

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

fromList :: [a] -> ST s (InputMaskin s a)
fromList xs = do
    m <- newSTRef xs
    let f = do
          (y:ys) <- readSTRef m
          writeSTRef m ys
          pure y
    pure $ InputMaskin f

mkComputer :: Vector Int
           -> ST s (Computer s)
mkComputer prog = do
    halted <- newSTRef False
    mem    <- V.thaw prog
    ip     <- newSTRef (Ip 0)
    pure $ Computer { isHalted = halted
                    , getMem   = mem
                    , getIp    = ip
                    }

part2 :: Vector Int -> Maybe Int
part2 prog = maximum
            . map (\ps -> runST $ runPart2 ps)
            . permutations
            $ [5..9]

    where
    runPart2 :: [Int] -> ST s (Maybe Int)
    runPart2 phases = do

        computers <- replicateM (length phases) (mkComputer prog)
        queues    <- mapM (\p -> queueFromList [p]) phases
        go Nothing (zip computers queues)

        where
        go :: Maybe Int -> [(Computer s, InputQueue s Int)] -> ST s (Maybe Int)
        go mPrev ((c,q):cqs) = do
            enqueue q $ fromMaybe 0 mPrev
            mo <- runUntilOutputOrHalt c (asInputMaskin q)
            case mo of
                Right () -> pure mPrev
                Left o -> do
                    let cqs' = cqs ++ [(c,q)]
                    go (Just o) cqs'

runUntilOutputOrHalt :: Computer s -> InputMaskin s Int -> ST s (Either Int ())
runUntilOutputOrHalt comp im = go =<< newSTRef Nothing
    where
    go out = do
        step comp im (OutputMaskin $ \a -> writeSTRef out (Just a))
        mv <- readSTRef out
        case mv of
            Just v -> pure $ Left v
            Nothing -> do
              halted <- readSTRef (isHalted comp)
              if halted
                  then pure (Right ())
                  else go out

part1 :: Vector Int -> Maybe Int
part1 prog = maximum
           . map (\ps -> runST $ runpart1 ps)
           . permutations
           $ [0..4]

    where
    runpart1 :: [Int] -> ST s (Maybe Int)
    runpart1 phases = do
        computers <- replicateM (length phases) (mkComputer prog)
        go Nothing computers phases
        where
        go :: Maybe Int -> [Computer s] -> [Int] -> ST s (Maybe Int)
        go mInp     []     [] = pure mInp
        go mInp (c:cs) (p:ps) = do
            let inp = fromMaybe 0 mInp
            [o] <- runFixedInputUntilHalted c [p, inp]
            go (Just o) cs ps

        runFixedInputUntilHalted :: Computer s -> [Int] -> ST s [Int]
        runFixedInputUntilHalted comp input = do
            im <- fromList input
            output <- newSTRef []
            let om = OutputMaskin $ \a -> modifySTRef' output (a:)
            run im om
            reverse <$> readSTRef output
            where
                run im om = do
                halted <- readSTRef . isHalted $ comp
                unless halted $ do
                    step comp im om
                    run im om

main :: IO ()
main = do
    prog <- V.fromList . (\input -> read ('[' : input ++ "]")) <$> readFile "./day7_input"
    print $ part1 prog
    print $ part2 prog
