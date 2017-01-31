module PiLcd
  ( PiLcd
  , mkPiLcd
  , getButtons
  ) where

import Control.Applicative
import Data.Word

import I2C
import Mcp23017

data PiLcd =
  PiLcd
  { plExpander :: PortExpander
  }

lcdAddr = 0x20

mkPiLcd :: I2cHandle -> IO PiLcd
mkPiLcd h =
  PiLcd <$> mkPortExpander (i2cReadReg h lcdAddr) (i2cWriteReg h lcdAddr)

getButtons :: PiLcd -> IO Word8
getButtons lcd = readGpioA (plExpander lcd)
