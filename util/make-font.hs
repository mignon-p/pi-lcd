#!/usr/bin/env runhaskell

{-# LANGUAGE MultiWayIf #-}

-- © Patrick Pelletier, 2017, BSD3

{-
Converts the file 5x8.bdf (available from
https://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html) to a more compact
binary representation.  Expects the file "5x8.bdf" to be in the current
directory, and writes the output file to "5x8.lcd" in the current
directory.

The 5x8.lcd file is a compact binary representation produced from the
original 5x8.bdf file.  Each character in 5x8.lcd takes up seven bytes.
The first two bytes are the Unicode code point, in big-endian order.
(The font only contains characters from the Basic Multilingual Plane, so
only two bytes are needed.)  The next five bytes are the bitmap data
for the glyph.  The glyph is stored "sideways", so that only five bytes
are needed instead of eight.  The characters must be sorted in ascending
order by code point, so that it is possible to binary search the file.
-}

import Data.Bits
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.Word
import System.Environment
import System.IO

encoding = "ENCODING "

word16ToWord8s :: Word16 -> [Word8]
word16ToWord8s w = [fromIntegral (w `shiftR` 8), fromIntegral w]

handleEncoding :: Handle -> String -> IO ()
handleEncoding hOut str = do
  let n = read str
      w8s = word16ToWord8s n
  B.hPut hOut $ B.pack w8s

handleHex :: String -> Integer -> Integer
handleHex str bits =
  let b = read ("0x" ++ str) :: Word8
  in (bits `shiftL` 5) .|. fromIntegral (b `shiftR` 3)

extractBits :: Integer -> Int -> Word8
extractBits bits pos = sum $ map f [0..7]
  where f n =
          let b = 1 .&. (bits `shiftR` (n * 5 + pos))
          in fromIntegral $ b `shiftL` n

handleEndChar :: Handle -> Integer -> IO ()
handleEndChar hOut bits = do
  let w8s = map (extractBits bits) [0..4]
  B.hPut hOut $ B.pack w8s

loop :: Handle -> Handle -> Integer -> IO ()
loop hIn hOut bits = do
  line <- hGetLine hIn
  if | encoding `isPrefixOf` line -> do
         handleEncoding hOut $ drop (length encoding) line
         loop hIn hOut 0
     | length line == 2 && isHexDigit (line !! 0) && isHexDigit (line !! 1) ->
         loop hIn hOut $ handleHex line bits
     | line == "ENDCHAR" -> do
         handleEndChar hOut bits
         loop hIn hOut 0
     | otherwise ->
         loop hIn hOut bits

main =
  withFile "5x8.bdf" ReadMode $ \hIn ->
    withFile "5x8.lcd" WriteMode $ \hOut -> loop hIn hOut 0
