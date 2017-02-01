#!/usr/bin/env runhaskell

{-# LANGUAGE MultiWayIf #-}

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
