module PiLcd
  ( PiLcd
  , LcdAddress(..)
  , defaultLcdAddress
  , Color(..)
  , Button(..)
  , ButtonDirection(..)
  , ButtonEvent(..)
  , openPiLcd
  , getButtons
  , buttonSelect
  , buttonRight
  , buttonDown
  , buttonUp
  , buttonLeft
  , getButtonEvent
  , setBacklightColor
  , updateDisplay
  , closePiLcd
  , turnOffAndClosePiLcd
  , charFromAsciiArt
  , LcdOptions(..)
  , defaultLcdOptions
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.IORef
import qualified Data.Text as T
import Data.Word
import System.Clock

import I2C
import LcdLowLevel
import Mcp23017
import qualified UnicodeLcd as U
import UnicodeLcd (charFromAsciiArt, LcdOptions(..), defaultLcdOptions)

data PiLcd =
  PiLcd
  { plHandle    :: I2cHandle
  , plExpander  :: PortExpander
  , plButtons   :: IORef Word8
  , plCallbacks :: LcdCallbacks
  , plLcd       :: U.Lcd
  }

data LcdAddress =
  LcdAddress
  { laBus :: Int
  , laAddr :: Int
  } deriving (Eq, Ord, Show, Read)

defaultLcdAddress :: LcdAddress
defaultLcdAddress =
  LcdAddress
  { laBus = 1
  , laAddr = 0x20
  }

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
colorValue Off     = off
colorValue Red     = red
colorValue Green   = green
colorValue Blue    = blue
colorValue Cyan    = cyan
colorValue Magenta = magenta
colorValue Yellow  = yellow
colorValue White   = white

buttonMaskA :: Word8
buttonMaskA = 0x1f

buttonMask :: Word16
buttonMask = fromIntegral buttonMaskA `shiftL` 8

data Button = ButtonSelect | ButtonRight | ButtonDown | ButtonUp | ButtonLeft
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

data ButtonDirection = Press | Release
                     deriving (Eq, Ord, Show, Read, Bounded, Enum)

data ButtonEvent = ButtonEvent Button ButtonDirection
                 deriving (Eq, Ord, Show, Read)

buttonList :: [Button]
buttonList = [ButtonSelect, ButtonRight, ButtonDown, ButtonUp, ButtonLeft]

bitSelect, bitRight, bitDown, bitUp, bitLeft :: Int
bitSelect = 0
bitRight  = 1
bitDown   = 2
bitUp     = 3
bitLeft   = 4

buttonSelect, buttonRight, buttonDown, buttonUp, buttonLeft :: Word8
buttonSelect = bit bitSelect
buttonRight  = bit bitRight
buttonDown   = bit bitDown
buttonUp     = bit bitUp
buttonLeft   = bit bitLeft

allBits :: Word16
allBits = 0xffff

openPiLcd :: LcdAddress -> LcdOptions -> IO PiLcd
openPiLcd la lo = do
  let lcdAddr = laAddr la
  h <- i2cOpen (laBus la)
  pe <- mkPortExpander (i2cReadReg h lcdAddr) (i2cWriteReg h lcdAddr)
  let outputs = white + 0xe0 -- rs, rw, e
  writeIoDir pe (complement outputs) allBits
  writeIPol  pe 0 allBits
  writeGpPu  pe buttonMask allBits
  but <- newIORef 0
  let cb = mkCallbacks pe
  lcdInitialize cb
  lcd <- U.mkLcd cb lo
  return $ PiLcd h pe but cb lcd

getButtons :: PiLcd -> IO Word8
getButtons lcd = do
  x <- readGpioA (plExpander lcd)
  return $ (x .&. buttonMaskA) `xor` buttonMaskA

findBit :: Word8 -> Int
findBit b = f 0
  where f n = if testBit b n
              then n
              else f (n + 1)

getButtonEvent :: PiLcd -> IO (Maybe ButtonEvent)
getButtonEvent lcd = do
  newButs <- getButtons lcd
  oldButs <- readIORef (plButtons lcd)
  let changedButs = newButs `xor` oldButs
  if changedButs == 0
    then return Nothing
    else do
      let aBit = findBit changedButs
          press = testBit newButs aBit
      writeIORef (plButtons lcd) (oldButs `xor` bit aBit)
      let dir = if press then Press else Release
      return $ Just $ ButtonEvent (buttonList !! aBit) dir

setBacklightColor :: PiLcd -> Color -> IO ()
setBacklightColor lcd c =
  writeGpio (plExpander lcd) (colorValue c `xor` white) white

-- reverse the bits in a nibble
reverseNibble :: Word8 -> Word8
reverseNibble x =
  -- https://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel
  let x' = ((x `shiftR` 1) .&. 5) .|. ((x .&. 5) `shiftL` 1)
  in ((x' `shiftR` 2) .&. 3) .|. ((x' .&. 3) `shiftL` 2)

mkByte :: LcdBus -> Word8
mkByte bus =
  bitIf (lbRS bus) 7 +
  bitIf (lbRW bus) 6 +
  bitIf (lbE  bus) 5 +
  case (lbDB bus) of
    Nothing -> 0
    (Just d) -> reverseNibble d `shiftL` 1

sendFunc :: PortExpander -> LcdBus -> IO ()
sendFunc pe bus = do
  let b = mkByte bus
  writeGpio pe (fromIntegral b) 0xfe
  let dataBits = 0x1e
      x = case (lbDB bus) of
            Nothing -> dataBits
            (Just _) -> 0
  writeIoDir pe x dataBits

recvFunc :: PortExpander -> IO Word8
recvFunc pe = do
  x <- readGpioB pe
  return $ reverseNibble $ (x `shiftR` 1) .&. 0xf

getNanos :: IO Integer
getNanos = toNanoSecs <$> getTime Monotonic

spin :: Int -> IO ()
spin nanos = do
  start <- getNanos
  let end = start + fromIntegral nanos
      sp = do
        now <- getNanos
        when (now < end) sp
  sp

delayFunc :: Int -> IO ()
delayFunc nanos =
  if nanos >= 10000
  then threadDelay $ (nanos + 999) `div` 1000
  else spin nanos

mkCallbacks :: PortExpander -> LcdCallbacks
mkCallbacks pe =
  LcdCallbacks
  { lcSend = sendFunc pe
  , lcRecv = recvFunc pe
  , lcDelay = delayFunc
  }

updateDisplay :: PiLcd -> [T.Text] -> IO ()
updateDisplay lcd txt = U.updateDisplay (plLcd lcd) txt

closePiLcd :: PiLcd -> IO ()
closePiLcd lcd = i2cClose (plHandle lcd)

turnOffAndClosePiLcd :: PiLcd -> IO ()
turnOffAndClosePiLcd lcd = do
  let cb = plCallbacks lcd
  lcdClear cb
  lcdControl cb False False False
  setBacklightColor lcd Off
  closePiLcd lcd
