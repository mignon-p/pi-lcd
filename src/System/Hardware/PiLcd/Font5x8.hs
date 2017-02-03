{-# LANGUAGE MultiWayIf #-}

module System.Hardware.PiLcd.Font5x8
  ( getCharacter
  , showCharacter
  ) where

import Control.Exception
import Data.Bits
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.Word
import Debug.Trace
import System.IO.Unsafe
import Text.Printf

font :: B.ByteString
font = unsafePerformIO $ do
  e <- try $ B.readFile "5x8.lcd"
  case e of
    (Left exc) -> handleExc exc
    (Right bs) -> return bs

handleExc :: IOException -> IO B.ByteString
handleExc _ = return B.empty

bytesPerChar :: Int
bytesPerChar = 7

getByte :: Int -> Int -> Word8
getByte n i = B.index font (n * bytesPerChar + i)

getCharacter :: Char -> Maybe [Word8]
getCharacter c = fmap decodeCharacter $ findCharacter c

findCharacter :: Char -> Maybe [Word8]
findCharacter c = do
  let nChars = B.length font `div` bytesPerChar
      cc = ord c
  n <- bSearch cc 0 nChars
  return $ map (getByte n) [bytesPerChar-1,bytesPerChar-2..2]

bSearch :: Int -> Int -> Int -> Maybe Int
bSearch _ _ 0 = Nothing
bSearch cc start len =
  let halfLen = len `div` 2
      mid = start + halfLen
      c = fromIntegral $ getCharCode mid
  in if | cc == c -> Just mid
        | cc < c -> bSearch cc start halfLen
        | cc > c -> bSearch cc (mid + 1) (len - halfLen - 1)

getCharCode :: Int -> Word16
getCharCode n = word8sToWord16 [getByte n 0, getByte n 1]

word8sToWord16 :: [Word8] -> Word16
word8sToWord16 [b1, b2] = (fromIntegral b1 `shiftL` 8) .|. fromIntegral b2

decodeCharacter :: [Word8] -> [Word8]
decodeCharacter ws = map (extractByte ws) [7,6..0]

extractByte :: [Word8] -> Int -> Word8
extractByte ws x = foldl' f 0 ws
  where f accum w = ((w `shiftR` x) .&. 1) .|. (accum `shiftL` 1)

showCharacter :: Char -> [String]
showCharacter c =
  let info = c : printf " U+%04X" (ord c)
  in case getCharacter c of
       Nothing -> [info ++ " not found"]
       (Just ws) -> (info ++ ":") : map showLine ws

showLine :: Word8 -> String
showLine w = map f [4,3..0]
  where f x = case 1 .&. (w `shiftR` x) of
                0 -> ' '
                1 -> '*'
