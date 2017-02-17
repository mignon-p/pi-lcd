{-# LANGUAGE OverloadedStrings #-}

-- There are potentially two different character set ROMs that
-- Adafruit might ship.  This lets you figure out which one you have.
-- I'd be interested in hearing from anyone with an A02 ROM, because
-- I have an A00 ROM.

-- This also demonstrates the "nativeChar" facility, where you can
-- specify a character in the LCD's native encoding, instead of Unicode.

-- This example is dedicated to the public domain (cc0) by Patrick Pelletier

import Data.Monoid
import qualified Data.Text as T
import Data.Word

import System.Hardware.PiLcd

mkLine :: T.Text -> Word8 -> T.Text
mkLine rom c =
  rom <> " if \"" <> T.singleton (nativeChar c) <> "\" is Pi"

main = do
  lcd <- openPiLcd defaultLcdAddress defaultLcdOptions
  updateDisplay lcd [ mkLine "A00" 0xf7
                    , mkLine "A02" 0x93
                    ]
  setBacklightColor lcd Cyan
  closePiLcd lcd
