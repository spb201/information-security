import System.Environment
import System.IO
import Data.Char
import Data.List
import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as B

import Asym
import Hash


main :: IO ()
main = do
    args <- getArgs
    let x:xs = args
    case x of
        "keys" -> do
            let bytes' : publicKeyFile : privateKeyFile : _ = xs
                bytes = read bytes'
            if bytes < 17
                then putStrLn "Minimum 17 bytes"
                else do
                    (publicKey, privateKey) <- Asym.generateKeys (bytes * 8)
                    writeFile publicKeyFile (show (bytes, publicKey))
                    writeFile privateKeyFile (show (bytes, privateKey))
        "sign" -> do
            let privateKeyFile : unsignedFile : signedFile : _ = xs
            contents <- B.readFile unsignedFile
            let hash = Hash.hashToInteger . Hash.md5 $ contents
            privateKeyString <- readFile privateKeyFile
            let (bytes, privateKey) = read privateKeyString
                signature = Asym.encrypt privateKey hash
                signatureString = signatureToByteString bytes signature
            B.writeFile signedFile (B.append contents signatureString)
        "check" -> do
            let publicKeyFile : signedFile : _ = xs
            contents <- B.readFile signedFile
            publicKeyString <- readFile publicKeyFile
            let (bytes, publicKey) = read publicKeyString
                len = B.length contents
                (fileContents, signatureString) = B.splitAt (len - bytes) contents
                hash = Hash.hashToInteger . Hash.md5 $ fileContents
                signature = byteStringToSignature signatureString
                hash' = Asym.decrypt publicKey signature
            if hash == hash'
                then putStrLn "Signature is valid"
                else putStrLn "Signature is invalid"
        "unsign" -> do
            let bytes' : signedFile : _ = xs
                bytes = read bytes'
            withBinaryFile signedFile ReadWriteMode (\handle -> do
                size <- hFileSize handle
                hSetFileSize handle (size - bytes)
                )


signatureToByteString :: Int -> Integer -> B.ByteString
signatureToByteString toSize =
    leftPad toSize 0 . B.unfoldr unfoldingFunction
  where
    unfoldingFunction a =
        if a > 0
            then Just (fromIntegral (a `rem` 256), a `div` 256)
            else Nothing

byteStringToSignature :: B.ByteString -> Integer
byteStringToSignature =
    B.foldr foldingFunction 0
  where
    foldingFunction byte acc = shift acc 8 + fromIntegral byte

leftPad :: Int -> Word8 -> B.ByteString -> B.ByteString
leftPad finalLength element message
    | messagePadded = message
    | otherwise     = leftPad finalLength element (element `B.cons` message)
  where messagePadded = fromIntegral (B.length message) >= finalLength

