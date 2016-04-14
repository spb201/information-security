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
            let publicKeyFile : privateKeyFile : _ = xs
            (publicKey, privateKey) <- Asym.generateKeys 256
            writeFile publicKeyFile (show publicKey)
            writeFile privateKeyFile (show privateKey)
        "sign" -> do
            let privateKeyFile : unsignedFile : signedFile : _ = xs
            contents <- B.readFile unsignedFile
            let hash = Hash.hashToInteger . Hash.md5 $ contents
            privateKeyString <- readFile privateKeyFile
            let privateKey = read privateKeyString
                signature = Asym.encrypt privateKey hash
                signatureString = signatureToByteString signature
            B.writeFile signedFile (B.append contents signatureString)
        "check" -> do
            let publicKeyFile : signedFile : _ = xs
            contents <- B.readFile signedFile
            publicKeyString <- readFile publicKeyFile
            let publicKey = read publicKeyString
                len = B.length contents
                (fileContents, signatureString) = B.splitAt (len - 32) contents
                hash = Hash.hashToInteger . Hash.md5 $ fileContents
                signature = byteStringToSignature signatureString
                hash' = Asym.decrypt publicKey signature
            if hash == hash'
                then putStrLn "OK"
                else putStrLn "ERROR"
        "unsign" -> do
            let signedFile : _ = xs
            withBinaryFile signedFile ReadWriteMode (\handle -> do
                size <- hFileSize handle
                hSetFileSize handle (size - 32)
                )


signatureToByteString :: Integer -> B.ByteString
signatureToByteString =
    leftPad 32 0 . B.unfoldr unfoldingFunction
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

