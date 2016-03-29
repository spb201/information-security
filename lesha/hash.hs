import Numeric
import Data.List
import Data.Word
import Data.Bits
import Data.Int
import Data.Array.IArray

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C


data Mode = Mode1 | Mode2 | Mode3 | Mode4
type Digest = (Word32, Word32, Word32, Word32)
type Operations =
    ( Digest -> Word32
    , Digest -> Word32
    , Digest -> Word32
    , Digest -> Word32
    )


-- HASH

hash :: Mode -> String -> Digest
hash mode input =
    foldl' foldingFunction initDigest chunks
  where
    message = fancyPad . C.pack $ input
    chunks = splitToChunks message

    foldingFunction :: Digest -> B.ByteString -> Digest
    foldingFunction (a, b, c, d) chunk =
        (a + a', b + b', c + c', d + d')
      where
        wordArray = breakChunk chunk
        (a', b', c', d') = foldl' (mainLoop wordArray) (a, b, c, d) [0..63]

    operations' = getOperations mode operations

    mainLoop :: Array Int Word32 -> Digest -> Int -> Digest
    mainLoop wordArray (a, b, c, d) i =
        (d, a', b, c)
      where
        f = operationResult operations' i (a, b, c, d)
        k = constants ! i
        word = wordArray ! wordIndex i
        rotateAmount = rotateAmounts ! i
        a' = b + rotate (a + f + k + word) rotateAmount


md5 :: String -> Digest
md5 = hash Mode1

-- PADDING MESSAGE

fancyPad :: B.ByteString -> B.ByteString
fancyPad message =
    B.append paddedMessage originalLengthBytes
  where
    paddedMessage = rightPadBytes' 64 56 . B.snoc message $ 0x80
    originalLengthBytes =
        rightPadBytes 8 . B.take 8 . toByteString $ B.length message * 8


rightPadBytes :: Int64 -> B.ByteString -> B.ByteString
rightPadBytes finalLength message
    | messagePadded = message
    | otherwise     = rightPadBytes finalLength . B.snoc message $ 0x00
  where messagePadded = B.length message >= finalLength


rightPadBytes' :: Int64 -> Int64 -> B.ByteString -> B.ByteString
rightPadBytes' divisor remainder message
    | messagePadded = message
    | otherwise     = rightPadBytes' divisor remainder . B.snoc message $ 0x00
  where messagePadded = B.length message `mod` divisor == remainder

-- BREAKING MESSAGE

toByteString :: Int64 -> B.ByteString
toByteString =
    B.unfoldr unfoldingFunction
  where
    unfoldingFunction a =
        if a > 0
            then Just (fromIntegral (a `rem` 256), a `div` 256)
            else Nothing


splitToChunks :: B.ByteString -> [B.ByteString]
splitToChunks = splitEveryBytes 64


breakChunk :: B.ByteString -> Array Int Word32
breakChunk chunk =
    listArray (0, 15) . map bytesToWord32 $ byteWords
  where
    byteWords = splitEveryBytes 4 chunk


splitEveryBytes :: Int -> B.ByteString -> [B.ByteString]
splitEveryBytes n string
    | B.null string = []
    | otherwise     = first : splitEveryBytes n second
  where (first, second) = B.splitAt (fromIntegral n) string


bytesToWord32 :: B.ByteString -> Word32
bytesToWord32 =
    B.foldr foldingFunction 0
  where
    foldingFunction byte acc = shift acc 8 + fromIntegral byte

-- MAIN LOOP

initDigest :: Digest
initDigest = (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476)


rotateAmounts :: Array Int Int
rotateAmounts = listArray (0, 63)
  [ 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22
  , 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20
  , 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23
  , 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
  ]


constants :: Array Int Word32
constants = array (0, 63)
    [ (i, k)
    | i <- [0..63]
    , let k = floor . (*) (2^32) . abs . sin . (+1) . fromIntegral $ i
    ]


operations :: Operations
operations =
    ( (\(_, b, c, d) -> (b .&. c) .|. ((complement b) .&. d))
    , (\(_, b, c, d) -> (d .&. b) .|. ((complement d) .&. c))
    , (\(_, b, c, d) -> b `xor` c `xor` d)
    , (\(_, b, c, d) -> c `xor` (b .|. (complement d)))
    )


getOperations :: Mode -> Operations -> Operations
getOperations Mode1 (f, g, h, k) = (f, g, h, k)
getOperations Mode2 (f, g, h, k) = (g, h, k, f)
getOperations Mode3 (f, g, h, k) = (h, k, f, g)
getOperations Mode4 (f, g, h, k) = (k, f, g, h)


operationResult :: Operations -> Int -> Digest -> Word32
operationResult (f, g, h, k) i digest
    | i < 16    = f digest
    | i < 32    = g digest
    | i < 48    = h digest
    | otherwise = k digest


wordIndex :: Int -> Int
wordIndex i
    | i < 16    = i
    | i < 32    = (5 * i + 1) `mod` 16
    | i < 48    = (3 * i + 5) `mod` 16
    | otherwise = (7 * i)     `mod` 16

-- CONVERTING

toHex :: Digest -> String
toHex (a, b, c, d) =
    concat . map (changeEndian . hexify) $ [a, b, c, d]
  where
    hexify = leftPad 8 '0' . flip showHex ""
    changeEndian = concat . reverse . splitEvery 2


leftPad :: Int -> a -> [a] -> [a]
leftPad finalLength element message
    | messagePadded = message
    | otherwise     = leftPad finalLength element (element:message)
  where messagePadded = length message >= finalLength


splitEvery :: Int -> [a] -> [[a]]
splitEvery n message
    | null message = []
    | otherwise    = first : splitEvery n second
  where (first, second) = splitAt n message

