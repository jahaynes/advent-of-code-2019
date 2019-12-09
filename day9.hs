{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad                     (unless)
import           Control.Monad.ST                  (ST, runST)
import           Data.STRef
import           Data.Vector.Unboxed.Mutable       (MVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Vector.Unboxed               (Vector)
import qualified Data.Vector.Unboxed         as V

newtype Ip = Ip Int

newtype Bp = Bp Int

data Mode = Position | Immediate | Relative

fromDigit :: Char -> Mode
fromDigit '0' = Position
fromDigit '1' = Immediate
fromDigit '2' = Relative
fromDigit _   = error "Unknown mode"

data Computer s =
    Computer { isHalted :: STRef s Bool
             , getMem   :: MVector s Int
             , getIp    :: STRef s Ip
             , getBp    :: STRef s Bp
             }

newtype InputMaskin s a =
    InputMaskin (ST s a)

newtype OutputMaskin s a =
    OutputMaskin (a -> ST s ())

plus :: Int -> Int -> Int
plus a b = do
    let c = a + b
        c' :: Integer = fromIntegral a + fromIntegral b
    if c' /= fromIntegral c
        then error "BIG INT"
        else c

times :: Int -> Int -> Int
times a b = do
    let c = a * b
        c' :: Integer = fromIntegral a * fromIntegral b
    if c' /= fromIntegral c
        then error "BIG INT"
        else c

step :: Computer s
     -> InputMaskin s Int
     -> OutputMaskin s Int
     -> ST s ()
step computer (InputMaskin readInput) (OutputMaskin writeOutput) = do

    let mem = getMem computer

    Ip ip <- readSTRef (getIp computer)
    Bp bp <- readSTRef (getBp computer)

    --3    rel    pos    pos
    (op, modeA, modeB, modeD) <- readOp mem (Ip ip)

    case op of
        1 -> do
            [src_a, src_b, dst] <- mapM (VM.read mem) $ map (plus ip) [1, 2, 3]
            a <- case modeA of
                     Position  -> VM.read mem src_a
                     Immediate -> pure src_a
                     Relative  -> VM.read mem (src_a `plus` bp)
            b <- case modeB of
                     Position  -> VM.read mem src_b
                     Immediate -> pure src_b
                     Relative  -> VM.read mem (src_b `plus` bp)
            case modeD of
                Position  -> VM.write mem dst $! a `plus` b
                Immediate -> error "Immediate dest!"
                Relative  -> VM.write mem (dst `plus` bp) $! a `plus` b
            writeSTRef (getIp computer) (Ip $ ip `plus` 4)

        2 -> do
            [src_a, src_b, dst] <- mapM (VM.read mem) $ map (plus ip) [1, 2, 3]
            a <- case modeA of
                     Position  -> VM.read mem src_a
                     Immediate -> pure src_a
                     Relative  -> VM.read mem (src_a `plus` bp)
            b <- case modeB of
                     Position  -> VM.read mem src_b
                     Immediate -> pure src_b
                     Relative  -> VM.read mem (src_b `plus` bp)
            case modeD of
                Position  -> VM.write mem dst $! a `times` b
                Immediate -> error "Immediate dest!"
                Relative  -> VM.write mem (dst `plus` bp) $! a `times` b

            writeSTRef (getIp computer) (Ip $ ip `plus` 4)

        3 -> do
            inp <- readInput
            dst <- VM.read mem (1 `plus` ip)
            case modeA of
                Position  -> VM.write mem dst inp
                Immediate -> error "Immediate dest!"
                Relative  -> VM.write mem (dst `plus` bp) inp
            writeSTRef (getIp computer) (Ip $ ip `plus` 2)

        4 -> do
            src_a <- VM.read mem (1 `plus` ip)
            a <- case modeA of
                     Position  -> VM.read mem src_a
                     Immediate -> pure src_a
                     Relative  -> VM.read mem (src_a `plus` bp)
            writeOutput a
            writeSTRef (getIp computer) (Ip $ ip `plus` 2)

        5 -> do
            [src_a, src_b] <- mapM (VM.read mem) $ map (plus ip) [1, 2]
            a <- case modeA of
                     Position  -> VM.read mem src_a
                     Immediate -> pure src_a
                     Relative  -> VM.read mem (src_a `plus` bp)
            b <- case modeB of
                     Position  -> VM.read mem src_b
                     Immediate -> pure src_b
                     Relative  -> VM.read mem (src_b `plus` bp)
            writeSTRef (getIp computer) (Ip $ if a /= 0 then b else ip `plus` 3)

        6 -> do
            [src_a, src_b] <- mapM (VM.read mem) $ map (plus ip) [1, 2]
            a <- case modeA of
                     Position  -> VM.read mem src_a
                     Immediate -> pure src_a
                     Relative  -> VM.read mem (src_a `plus` bp)
            b <- case modeB of
                     Position  -> VM.read mem src_b
                     Immediate -> pure src_b
                     Relative  -> VM.read mem (src_b `plus` bp)
            writeSTRef (getIp computer) (Ip $ if a == 0 then b else ip `plus` 3)

        7 -> do
            [src_a, src_b, dst] <- mapM (VM.read mem) $ map (plus ip) [1, 2, 3]
            a <- case modeA of
                        Position  -> VM.read mem src_a
                        Immediate -> pure src_a
                        Relative  -> VM.read mem (src_a `plus` bp)
            b <- case modeB of
                        Position  -> VM.read mem src_b
                        Immediate -> pure src_b
                        Relative  -> VM.read mem (src_b `plus` bp)
            let lt = if a < b then 1 else 0
            case modeD of
                Position  -> VM.write mem dst $! lt
                Immediate -> error "Immediate dest!"
                Relative  -> VM.write mem (dst `plus` bp) $! lt
            writeSTRef (getIp computer) (Ip $ ip `plus` 4)

        8 -> do
            [src_a, src_b, dst] <- mapM (VM.read mem) $ map (plus ip) [1, 2, 3]
            a <- case modeA of
                        Position  -> VM.read mem src_a
                        Immediate -> pure src_a
                        Relative  -> VM.read mem (src_a `plus` bp)
            b <- case modeB of
                        Position  -> VM.read mem src_b
                        Immediate -> pure src_b
                        Relative  -> VM.read mem (src_b `plus` bp)
            let eq = if a == b then 1 else 0
            case modeD of
                Position  -> VM.write mem dst $! eq
                Immediate -> error "Immediate dest!"
                Relative  -> VM.write mem (dst `plus` bp) $! eq
            writeSTRef (getIp computer) (Ip $! ip `plus` 4)

        9 -> do
            src_a <- VM.read mem (1 `plus` ip)
            a <- case modeA of
                        Position  -> VM.read mem src_a
                        Immediate -> pure src_a
                        Relative  -> VM.read mem (src_a `plus` bp)
            writeSTRef (getBp computer) (Bp $! bp `plus` a)
            writeSTRef (getIp computer) (Ip $! ip `plus` 2)

        99 -> writeSTRef (isHalted computer) True

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

    mem   <- VM.new 100000
    VM.copy (VM.slice 0 (V.length prog) mem) =<< V.thaw prog

    halted <- newSTRef False
    ip     <- newSTRef (Ip 0)
    bp     <- newSTRef (Bp 0)
    pure $ Computer { isHalted = halted
                    , getMem   = mem
                    , getIp    = ip
                    , getBp    = bp
                    }

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

part :: Int -> Vector Int -> [Int]
part n prog = runST $ do
    comp <- mkComputer prog
    runFixedInputUntilHalted comp [n]

main :: IO ()
main = do
    prog <- V.fromList . (\input -> read ('[' : input ++ "]")) <$> readFile "./day9_input"
    print $ part 1 prog
    print $ part 2 prog
