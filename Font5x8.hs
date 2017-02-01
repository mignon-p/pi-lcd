module Font5x8 where

import qualified Data.ByteString as B
import System.IO.Unsafe

font :: B.ByteString
font = unsafePerformIO $ B.readFile "5x8.lcd"

getCharacter :: Char -> Maybe [Word8]
getCharacter c = fmap decodeCharacter $ findCharacter c

findCharacter :: Char -> Maybe [Word8]
findCharacter
