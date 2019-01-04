-- Benchmarks for Curry systems

import Debug.Profile

-- Standard Prolog benchmark: naive reverse:

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]

-- start naive reverse benchmark with a list of n elements and report
-- space usage:
nrev :: Int -> IO ()
nrev n = do
  let xs = [1 .. n]
  const done $!! xs
  profileSpaceNF (rev xs)
-- LIPS = (n+1)*(n+2)/2/exec.time

-- compute LIPS with naive reverse benchmark:
nrevLIPS :: Int -> IO ()
nrevLIPS n = do
  let xs = [1 .. n]
  const done $!! xs
  garbageCollect
  garbageCollectorOff
  pi1 <- getProcessInfos
  const done $!! (rev xs)
  pi2 <- getProcessInfos
  garbageCollectorOn
  let rtime = maybe 0 id (lookup RunTime pi2)
              - maybe 0 id (lookup RunTime pi1)
  -- LIPS = (n+1)*(n+2)/2/exec.time
  putStrLn $ "LIPS: " ++ show ((n+1)*(n+2)*1000 `div` (2*rtime))

main :: IO ()
main = nrevLIPS 4000

-- Result on a Sun Ultra-10 (chevalblanc, with Sicstus/Fast code):
-- 2.5 MLIPS for (nrev 1000)

-- Result on a Linux-PC PIII/650Mhz (with Sicstus/Emulated code):
-- 0.94 MLIPS for (nrev 1000)

-- Result on a Linux-PC AMD Athlon/900Mhz (with Sicstus/Emulated code):
-- 1.12 MLIPS for (nrev 1000)

-- Result on a Linux-PC AMD Athlon/1.300Mhz (with Sicstus/Emulated code):
-- 1.43 MLIPS for (nrev 1000)

-- Result on a Linux-PC AMD Athlon XP 2600+/2.000Mhz (petrus, Sicstus/Emulated):
-- 2.95 MLIPS for (nrev 1000)

-- Result on a Linux-PC Intel Core i7-4790 / 3.6Ghz (belair, Sicstus 4.3/JIT):
-- 13.45 MLIPS for (nrev 4000)


-- as nrev but double evaluation
nrev2 :: Int -> IO ()
nrev2 n = do
  let xs = [1 .. n]
  const done $!! xs
  profileSpaceNF (rev xs, rev xs)

-- as nrev2 but with test equality instead of unification:
nrev3 :: Int -> IO ()
nrev3 n = do
  let xs = [1 .. n]
  const done $!! xs
  profileSpaceNF (rev xs == rev xs)
