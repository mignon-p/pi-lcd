{-# LANGUAGE MultiWayIf #-}

module System.Hardware.PiLcd.Util
  ( padLine
  , bitIf
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
