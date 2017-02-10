{-# LANGUAGE MultiWayIf #-}

{-|
Module      : System.Hardware.PiLcd.Font5x8
Description : A 5x8 font of Unicode characters
Copyright   : © Patrick Pelletier, 2017
License     : BSD3
Maintainer  : code@funwithsoftware.org

Given a Unicode code point, returns a glyph made up of 5x8 dots.
Only supports characters which are made up of a single code point.
(In other words, combining marks are not supported.)

Uses the <https://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html 5x8 fixed font>,
originally from the X Window System.  This font is in the public domain.
The font is stored in a compact binary representation in the file
@5x8.lcd@, which is automatically loaded (via 'unsafePerformIO')
when needed.
-}

module System.Hardware.PiLcd.Font5x8
  ( getCharacter
  , showCharacter
  , charFromAsciiArt
  ) where

import Control.Exception
import Data.Bits
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.Word
import System.IO.Unsafe
import Text.Printf

import Paths_pi_lcd
import System.Hardware.PiLcd.Util

{-
The 5x8.lcd file is a compact binary representation produced from the
original 5x8.bdf file.  Each character in 5x8.lcd takes up seven bytes.
The first two bytes are the Unicode code point, in big-endian order.
(The font only contains characters from the Basic Multilingual Plane, so
only two bytes are needed.)  The next five bytes are the bitmap data
for the glyph.  The glyph is stored "sideways", so that only five bytes
are needed instead of eight.  The characters must be sorted in ascending
order by code point, so that it is possible to binary search the file.
-}

font :: B.ByteString
font = unsafePerformIO $ do
  path <- getDataFileName "5x8.lcd"
  e <- try $ B.readFile path
  case e of
    (Left exc) -> handleExc exc
    (Right bs) -> return bs

handleExc :: IOException -> IO B.ByteString
handleExc _ = return B.empty

bytesPerChar :: Int
bytesPerChar = 7

getByte :: Int -> Int -> Word8
getByte n i = B.index font (n * bytesPerChar + i)

-- | Looks up the given character, and returns the 5x8 bitmap
-- for the character.  If the character is not found in the font,
-- returns 'Nothing'.  If the character is found, returns a list
-- of eight bytes, one for each row of the glyph.  Within each byte,
-- the least significant five bits contain the five dots for that row.
-- This is the format expected by the HD44780 for custom characters.
getCharacter :: Char -> Maybe [Word8]
getCharacter c =
  case c `lookup` extraChars of
    x@(Just _) -> x
    Nothing -> getCharacter' c

getCharacter' :: Char -> Maybe [Word8]
getCharacter' c = fmap decodeCharacter $ findCharacter c

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

decodeCharacter :: [Word8] -> [Word8]
decodeCharacter ws = map (extractByte ws) [7,6..0]

extractByte :: [Word8] -> Int -> Word8
extractByte ws x = foldl' f 0 ws
  where f accum w = ((w `shiftR` x) .&. 1) .|. (accum `shiftL` 1)

-- | A quick and dirty way to visualize characters in the font.
-- Given a character, returns the glyph as ASCII art.
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

-- | Converts a glyph from ASCII art to binary representation.
-- Expects a list of eight lines, where each line contains five
-- characters.  A dot is considered \"off\" if the character is
-- a space, or \"on\" if it is any other character.
-- Returns the glyph in the format expected for custom characters:
-- eight bytes where each byte contains data in the least significant
-- five bits.
charFromAsciiArt :: [String] -> [Word8]
charFromAsciiArt ls = map f ls
  where f s = foldl' g 0 s
        g accum c = (accum `shiftL` 1) + case c of
                                           ' ' -> 0
                                           _   -> 1

extraChars :: [(Char, [Word8])]
extraChars =
  [ ( '↕'
    , charFromAsciiArt
      -- this one has bigger arrowheads than the one in the font
      [ "  *  "
      , " *** "
      , "* * *"
      , "  *  "
      , "  *  "
      , "* * *"
      , " *** "
      , "  *  "
      ]
    ) ]
