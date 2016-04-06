import System.Random
import Data.Bits
import Control.Monad.Fix
import Data.List
import Data.Word

import System.Environment


primes =
    [ 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61
    , 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131
    , 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197
    , 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271
    , 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353
    , 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433
    , 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509
    , 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601
    , 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677
    , 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769
    , 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859
    , 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953
    , 967, 971, 977, 983, 991, 997, 1009, 1013, 1019, 1021, 1031, 1033
    , 1039, 1049, 1051, 1061, 1063, 1069, 1087, 1091, 1093, 1097, 1103
    , 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181, 1187, 1193
    , 1201, 1213, 1217, 1223, 1229, 1231, 1237, 1249, 1259, 1277, 1279
    , 1283, 1289, 1291, 1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361
    , 1367, 1373, 1381, 1399, 1409, 1423, 1427, 1429, 1433, 1439, 1447
    , 1451, 1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511
    , 1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597
    , 1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657, 1663, 1667
    , 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, 1741, 1747, 1753
    , 1759, 1777, 1783, 1787, 1789, 1801, 1811, 1823, 1831, 1847, 1861
    , 1867, 1871, 1873, 1877, 1879, 1889, 1901, 1907, 1913, 1931, 1933
    , 1949, 1951, 1973, 1979, 1987, 1993, 1997, 1999 ]


rndPrime :: Int -> IO Integer
rndPrime bits =
    fix $ \again -> do
        x <- fmap (.|. 1) $ randomRIO (2^(bits - 1), 2^bits - 1)
        isPr <- isPrime x
        if isPr then return x else again


rndPrimes :: Int -> IO (Integer, Integer)
rndPrimes bits = do
    p <- rndPrime bits
    fix $ \again -> do
        q <- rndPrime bits
        if p /= q
            then return (p, q)
            else again


millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a <= 1 || a >= n-1 = error $ "millerRabinPrimality"
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
  where
    n' = n-1
    (k,m) = find2km n'
    b0 = fastPow a m n
    b = take (fromIntegral k) $ iterate (squareMod n) b0
    iter [] = False
    iter (x:xs)
        | x == 1 = False
        | x == n' = True
        | otherwise = iter xs


find2km :: Integral a => a -> (a,a)
find2km n =
    f 0 n
  where
    f k m
        | r == 1 = (k,m)
        | otherwise = f (k+1) q
      where (q,r) = quotRem m 2


squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a


isPrime :: Integer -> IO Bool
isPrime n = do
    witnesses <- sequence [randomRIO (0, n `div` 2), randomRIO (0, n `div` 2),
        randomRIO (0, n `div` 2), randomRIO (0, n `div` 2),
        randomRIO (0, n `div` 2)]
    return ((isPrimeDiv n) && (isPrimeMR n witnesses))
  where
    isPrimeDiv x = all (\prime -> x `mod` prime /= 0) primes
    isPrimeMR x ws = all (millerRabinPrimality x) ws

n :: Integer -> Integer -> Integer
n = (*)

φ :: Integer -> Integer -> Integer
φ p q = (p - 1) * (q - 1)

e' = 65537

-- ax + by = g
gcdExt a 0 = (1, 0, a)
gcdExt a b =
    (t, s - q * t, g)
  where
    (q, r) = a `quotRem` b
    (s, t, g) = gcdExt b r


modInv a m =
    if g == 1
        then mkPos i
        else 0
  where
    (i, _, g) = gcdExt a m
    mkPos x = if x < 0 then x + m else x

fastPow :: Integral a => a -> a -> a -> a
fastPow base 1 m = mod base m
fastPow base pow m
    | even pow = mod ((fastPow base (div pow 2) m) ^ 2) m
    | odd  pow = mod ((fastPow base (div (pow-1) 2) m) ^ 2 * base) m


d :: Integer -> Integer -> Integer -> Integer
d e p q = modInv e (φ p q)


openKey :: Integer -> Integer -> Integer -> (Integer, Integer)
openKey e p q = (e, n p q)


closedKey :: Integer -> Integer -> Integer -> (Integer, Integer)
closedKey e p q = (d e p q, n p q)


main :: IO ()
main = do
    args <- getArgs
    let x:xs = args
    case x of
        "k" -> do
            let y:ys = xs
            pq <- rndPrimes . (`div` 2) . read $ y
            let p = fst pq
                q = snd pq
                ok = openKey e' p q
                ck = closedKey e' p q
            putStrLn "Open key"
            putStrLn $ y ++ " " ++ (show (fst ok)) ++ " "++ (show (snd ok))
            putStrLn ""
            putStrLn "Closed key"
            putStrLn $ y ++ " " ++ (show (fst ck)) ++ " " ++ (show (snd ck))
            putStrLn ""

