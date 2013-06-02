module SSSS where

import Data.Bits
import Data.Char
import Data.List
import Data.Ratio
import Data.Word
import qualified System.Random as Random

type Key = (Integer, Integer)

main = do
  rng <- Random.getStdGen
  let k = 3
  let keys = take 10 $ split rng k "secret"
  mapM (putStrLn . show . \(n, s) -> (n, decode s)) keys
  putStrLn $ show $ join $ take k $ keys

--prime = 257
prime = 9223372036854775783
--prime = shift 1 607 - 1

split :: Random.StdGen -> Int -> String -> [Key]
split rng k msg =
  [(x, fn x `mod` prime) | x <- [1 .. prime - 1]]
  where
    cs = take (k - 1) $ Random.randomRs (1, prime - 1) rng
    term x exp c = c * (x ^ exp `mod` prime) `mod` prime
    fn x = sum $ zipWith (term x) [0..] (encode msg : cs)

--splitIO :: Int -> String -> IO [Key]
splitIO k msg = Random.getStdGen >>= \rng -> return $ split rng k msg

join :: [Key] -> String
join ks =
  decode $ round $ approxRational (sum [freeCoefficient k * (snd k % 1) | k <- ks]) 0.5
  where
    denoms k = zipWith (-) (repeat $ fst k) $ map fst $ delete k ks
    numerators k = map (negate . fst) $ delete k ks
    freeCoefficient k = product $ zipWith (%) (numerators k) (denoms k)

encode :: String -> Integer
encode =
  foldr (.|.) 0 . zipWith shiftBytes [0..] . reverse . map (int2word8 . ord)
  where
    shiftBytes exp x = shift (fromIntegral x :: Integer) (8 * exp)
    int2word8 i
      | 0 <= i && i <= 255 = fromIntegral i :: Word8
      | otherwise = error "only ASCII strings may be encoded"

decode :: Integer -> String
decode =
  reverse . unfoldr lastByte
  where
    lastByte 0 = Nothing
    lastByte x = Just (chr (fromIntegral x .&. 0xFF), shift x (-8))

--showBits x = showIntAtBase 2 (chr . (+ 48)) x ""
