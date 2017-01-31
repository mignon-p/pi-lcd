module PiLcd
  ( PiLcd
  , Color(..)
  , mkPiLcd
  , getButtons
  , setBacklightColor
  ) where

import Control.Applicative
import Data.Bits
import Data.Word

import I2C
import Mcp23017

data PiLcd =
  PiLcd
  { plExpander :: PortExpander
  }

lcdAddr = 0x20

data Color = Off | Red | Green | Blue | Cyan | Magenta | Yellow | White
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

off, red, green, blue, cyan, magenta, yellow, white :: Word16

off     = 0
red     = bit 14
green   = bit 15
blue    = bit 0
cyan    = green + blue
magenta = red + blue
yellow  = red + green
white   = red + green + blue

colorValue :: Color -> Word16
colorValue Off = off
colorValue Red = red
colorValue Green = green
colorValue Blue = blue
colorValue Cyan = cyan
colorValue Magenta = magenta
colorValue Yellow = yellow
colorValue White = white

mkPiLcd :: I2cHandle -> IO PiLcd
mkPiLcd h =
  PiLcd <$> mkPortExpander (i2cReadReg h lcdAddr) (i2cWriteReg h lcdAddr)

getButtons :: PiLcd -> IO Word8
getButtons lcd = readGpioA (plExpander lcd)

setBacklightColor :: PiLcd -> Color -> IO ()
setBacklightColor lcd c = writeGpio (plExpander lcd) (colorValue c) white
