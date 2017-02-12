{-# LANGUAGE OverloadedStrings #-}

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
