{-|
Module      : System.Hardware.PiLcd
Description : Control an Adafruit character LCD and keypad kit
Copyright   : © Patrick Pelletier, 2017
License     : BSD3
Maintainer  : code@funwithsoftware.org
Stability   : experimental
Portability : Linux

This module contains everything you need to use an
<https://www.adafruit.com/categories/808 Adafruit character LCD and keypad kit>
from Haskell.
-}

module System.Hardware.PiLcd
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
  , UiData(..)
  , UiState(..)
  , InternalState
  , defaultUiState
  , runUi
  , runUiUntilDone
  , withPiLcd
  , withPiLcdThenTurnOff
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Data.IORef
import Data.Monoid
import qualified Data.Text as T
import Data.Word
import System.Clock

import System.Hardware.PiLcd.Font5x8
import System.Hardware.PiLcd.Hd44780
import System.Hardware.PiLcd.I2c
import System.Hardware.PiLcd.Mcp23017
import qualified System.Hardware.PiLcd.UnicodeLcd as U
import System.Hardware.PiLcd.UnicodeLcd
  (LcdOptions(..), defaultLcdOptions)
import qualified System.Hardware.PiLcd.UserInterface as UI
import System.Hardware.PiLcd.UserInterface
  (Button(..), ButtonDirection(..), ButtonEvent(..),
   UiData(..), UiState(..), InternalState, defaultUiState)
import System.Hardware.PiLcd.Util

-- | An opaque type representing an LCD and keypad kit.
data PiLcd =
  PiLcd
  { plHandle    :: I2cHandle
  , plExpander  :: PortExpander
  , plButtons   :: IORef Word8
  , plCallbacks :: LcdCallbacks
  , plLcd       :: U.Lcd
  }

-- | Specifies how to connect to the LCD+keypad kit.  'laBus' should be 1 for
-- revision 2 Raspberry Pis and later.  For revision 1 Pis (those with 256 MB
-- of RAM), the bus should be 0.  If you need a way to automatically detect
-- this, consider using the @piBoardRev@ function in the
-- <https://hackage.haskell.org/package/wiringPi wiringPi package>.
-- On the other hand, there is not much reason to ever change
-- 'laAddr' from the default 0x20.
data LcdAddress =
  LcdAddress
  { laBus :: Int  -- ^ The I2C bus to communicate on
  , laAddr :: Int -- ^ The address on that bus to find the LCD+keypad kit
  } deriving (Eq, Ord, Show, Read)

-- | Default values for 'LcdAddress'.  Defaults to bus 1 and address 0x20.
defaultLcdAddress :: LcdAddress
defaultLcdAddress =
  LcdAddress
  { laBus = 1
  , laAddr = 0x20
  }

-- | Colors for the LED backlight.  If you have a single-color
-- backlight, just use 'Off' and 'White' to turn it off and on.
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

-- | Opens the LCD+keypad kit, at the specified address, with the
-- specified options.
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

-- | Returns all of the buttons which are currently depressed, as a
-- bitwise \"or\" of 'buttonSelect', 'buttonRight', 'buttonDown',
-- 'buttonUp', and 'buttonLeft'.
getButtons :: PiLcd -> IO Word8
getButtons lcd = do
  x <- readGpioA (plExpander lcd)
  return $ (x .&. buttonMaskA) `xor` buttonMaskA

findBit :: Word8 -> Int
findBit b = f 0
  where f n = if testBit b n
              then n
              else f (n + 1)

-- | If a button has been pressed or released since the last call to
-- 'getButtonEvent', returns information on that press or release as
-- a 'ButtonEvent'.
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

-- | Set the LED backlight to one of the eight possible 'Color' values.
-- If you have a single-color backlight, just use 'Off' and 'White' to
-- turn it off and on.
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

-- | Update the display to contain the specified lines
-- of text.  This is done intelligently; i. e. only the
-- characters which have changed are rewritten.
-- The lines are truncated or padded with spaces to make
-- them the width of the display.  Similarly, the list
-- of lines is truncated or padded with blank lines to
-- make it the height of the display.
updateDisplay :: PiLcd -> [T.Text] -> IO ()
updateDisplay lcd txt = U.updateDisplay (plLcd lcd) txt

-- | Closes the 'PiLcd', leaving the display contents and
-- backlight setting untouched.
closePiLcd :: PiLcd -> IO ()
closePiLcd lcd = i2cClose (plHandle lcd)

-- | Like 'closePiLcd', but clears the display, turns off the
-- display, and turns off the backlight before closing the 'PiLcd'.
turnOffAndClosePiLcd :: PiLcd -> IO ()
turnOffAndClosePiLcd lcd = do
  let cb = plCallbacks lcd
  lcdClear cb
  lcdControl cb False False False
  setBacklightColor lcd Off
  closePiLcd lcd

runUi :: PiLcd
      -> UiData
      -> UiState
      -> IO (UiState, Bool)
runUi lcd dat st = do
  mbe <- getButtonEvent lcd
  let columns = loColumns $ U.lcdOptions $ plLcd lcd
      (ls, st', done) = UI.runUi dat st mbe columns
  updateDisplay lcd ls
  return (st', done)

runUiUntilDone :: PiLcd
               -> UiData
               -> UiState
               -> IO UiState
runUiUntilDone lcd dat st = do
  (st', done) <- runUi lcd dat st
  if done
    then return st'
    else do
    threadDelay 20000
    runUiUntilDone lcd dat st'

withPiLcd :: LcdAddress
          -> LcdOptions
          -> (PiLcd -> IO a)
          -> IO a
withPiLcd = withPiLcd' closePiLcd

withPiLcdThenTurnOff :: LcdAddress
                     -> LcdOptions
                     -> (PiLcd -> IO a)
                     -> IO a
withPiLcdThenTurnOff = withPiLcd' turnOffAndClosePiLcd

wrapLine :: Int -> T.Text -> [T.Text]
wrapLine columns txt
  | T.length txt <= columns = [txt]
  | otherwise = let (first, rest) = T.splitAt columns txt
                in first : wrapLine columns rest

displayException :: PiLcd -> SomeException -> IO ()
displayException lcd se = do
  let columns = loColumns $ U.lcdOptions $ plLcd lcd
      rows = loLines $ U.lcdOptions $ plLcd lcd
      txt = padLine (columns * rows) $ T.pack $ show se
      txts = wrapLine columns txt
  setBacklightColor lcd Red
  updateDisplay lcd txts

withPiLcd' :: (PiLcd -> IO ())
           -> LcdAddress
           -> LcdOptions
           -> (PiLcd -> IO a)
           -> IO a
withPiLcd' closeFunc la lo body = do
  lcd <- openPiLcd la lo
  eth <- try (body lcd)
  case eth of
    Left e -> do
      displayException lcd e
      closePiLcd lcd
      throwIO e
    Right x -> do
      closeFunc lcd
      return x
