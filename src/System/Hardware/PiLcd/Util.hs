{-# LANGUAGE MultiWayIf #-}

-- © Patrick Pelletier, 2017, BSD3

module System.Hardware.PiLcd.Util
  ( padLine
  , bitIf
  , word8sToWord16
  , word16ToWord8s
  ) where

import Data.Bits
import Data.Monoid
import qualified Data.Text as T
import Data.Word

space :: T.Text
space = T.singleton ' '

padLine :: Int -> T.Text -> T.Text
padLine columns txt =
  let len = T.length txt
  in if | len < columns -> txt <> T.replicate (columns - len) space
        | len > columns -> T.take columns txt
        | otherwise -> txt

bitIf :: Bool -> Int -> Word8
bitIf b n = if b then bit n else 0

word8sToWord16 :: [Word8] -> Word16
word8sToWord16 [b1, b2] = (fromIntegral b1 `shiftL` 8) .|. fromIntegral b2

word16ToWord8s :: Word16 -> [Word8]
word16ToWord8s w = [fromIntegral (w `shiftR` 8), fromIntegral w]
