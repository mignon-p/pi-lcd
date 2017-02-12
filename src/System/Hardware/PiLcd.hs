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
  ( -- * Creating a PiLcd
    openPiLcd
  , closePiLcd
  , turnOffAndClosePiLcd
  , PiLcd
  , LcdAddress(..)
  , defaultLcdAddress
  , LcdOptions(..)
  , RomCode(..)
  , defaultLcdOptions
    -- * Backlight color
  , Color(..)
  , setBacklightColor
    -- * Buttons
  , Button(..)
  , ButtonDirection(..)
  , ButtonEvent(..)
  , getButtonEvent
  , getButtons
    -- ** Button bitmask values
  , buttonSelect
  , buttonRight
  , buttonDown
  , buttonUp
  , buttonLeft
    -- * Display
    -- | Displays Unicode text on an LCD.  Only updates the parts
    -- of the LCD which have changed.  Automatically manages
    -- custom characters, using the
    -- <https://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html 5x8 fixed font>
    -- for characters which are not built-in to the the LCD controller's ROM.
    -- Only eight distinct non-built-in characters can be on the display
    -- at any one time.
    --
    -- Only supports characters which are made up of a single code point.
    -- (In other words, combining marks are not supported.)  If your input
    -- contains decomposed characters, consider using the
    -- <https://hackage.haskell.org/package/unicode-transforms unicode-transforms>
    -- package to convert to Normalization Form C.
  , updateDisplay
  , charFromAsciiArt
  , nativeChar
    -- * User Interface
    -- | Displays a simple user interface.  The first line of the display
    -- is used as a \"list box\", where the user can scroll through a
    -- list of items one at a time using the up and down buttons.
    -- The second line of the display is used for virtual \"buttons\",
    -- such as \"OK\" and \"Cancel\".  The user uses the left and right buttons
    -- to select a virtual \"button\".  When the user presses the
    -- Select button, the interaction is considered done, and the calling
    -- program is given the list item and button selection that the user
    -- made.
  , UiData(..)
  , UiState(..)
  , InternalState
  , defaultUiState
  , runUi
  , runUiUntilDone
    -- * Exception handling
    -- | These are specialized forms of 'bracket', where an uncaught
    -- exception causes the backlight to turn red and the exception to
    -- be displayed on the LCD.  Then the exception is rethrown.  This
    -- is useful for headless setups, where the LCD is the primary
    -- means of user interface.
  , withPiLcd
  , withPiLcdThenTurnOff
  ) where

import Control.Concurrent
import Control.Exception
import Data.Bits
import Data.IORef
import qualified Data.Text as T
import Data.Word

import System.Hardware.PiLcd.Font5x8
import System.Hardware.PiLcd.Hd44780
import System.Hardware.PiLcd.I2c
import System.Hardware.PiLcd.Mcp23017
import qualified System.Hardware.PiLcd.UnicodeLcd as U
import System.Hardware.PiLcd.UnicodeLcd
  (LcdOptions(..), defaultLcdOptions, RomCode(..), nativeChar)
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

lcdBits :: Word16
lcdBits = 0x00fe -- rs, rw, e, db4-db7

-- | Opens the LCD+keypad kit, at the specified address, with the
-- specified options.
openPiLcd :: LcdAddress -> LcdOptions -> IO PiLcd
openPiLcd la lo = do
  let lcdAddr = laAddr la
  h <- i2cOpen (laBus la)
  pe <- mkPortExpander (i2cReadReg h lcdAddr) (i2cWriteReg h lcdAddr)
  let outputs = white + lcdBits
  writeIoDir pe (complement outputs) allBits
  writeIPol  pe 0 allBits
  writeGpPu  pe buttonMask allBits
  writeGpio  pe 0 lcdBits
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
  (reverseNibble (lbDB bus) `shiftL` 1)

sendFunc :: PortExpander -> LcdBus -> IO ()
sendFunc pe bus = do
  let b = mkByte bus
  writeGpio pe (fromIntegral b) lcdBits

mkCallbacks :: PortExpander -> LcdCallbacks
mkCallbacks pe =
  LcdCallbacks
  { lcSend = sendFunc pe
  }

-- | Update the display to contain the specified lines
-- of text.  This is done intelligently; i. e. only the
-- characters which have changed are rewritten.
-- The lines are truncated or padded with spaces to make
-- them the width of the display.  Similarly, the list
-- of lines is truncated or padded with blank lines to
-- make it the height of the display.
updateDisplay :: PiLcd -> [T.Text] -> IO ()
updateDisplay lcd = U.updateDisplay (plLcd lcd)

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

-- | Updates the display based on the given UI state, and updates
-- the UI state based on a button press or release which may have
-- occurred since the last call.
runUi :: PiLcd
      -> UiData             -- ^ Data to display in the UI
      -> UiState            -- ^ Current state of the interaction
      -> IO (UiState, Bool) -- ^ New UI state, and a flag indicating
                            -- whether the interaction is done (i. e. user has
                            -- pressed and released the \"Select\" button)
runUi lcd dat st = do
  mbe <- getButtonEvent lcd
  let columns = loColumns $ U.lcdOptions $ plLcd lcd
      (ls, st', done) = UI.runUi dat st mbe columns
  updateDisplay lcd ls
  return (st', done)

-- | Calls 'runUi' repeatedly, with a short delay in between, feeding back
-- the state, until the interaction is \"done\".  (i. e. user has
-- pressed and released the \"Select\" button)  The final state is
-- returned, which indicates the selection the user has made.
runUiUntilDone :: PiLcd
               -> UiData  -- ^ Data to display in the UI
               -> UiState -- ^ Initial state (i. e. which list item and button
                          -- start out highlighted)
               -> IO UiState -- ^ Final state (selections user made)
runUiUntilDone lcd dat st = do
  (st', done) <- runUi lcd dat st
  if done
    then return st'
    else do
    threadDelay 20000
    runUiUntilDone lcd dat st'

-- | Opens a 'PiLcd' with the given 'LcdAddress' and 'LcdOptions', passes
-- the 'PiLcd' to the body computation, and then closes the 'PiLcd',
-- regardless of whether the body exited normally or exceptionally.
-- If an exception occurred, the exception is shown on the LCD.
withPiLcd :: LcdAddress
          -> LcdOptions
          -> (PiLcd -> IO a) -- ^ Body computation
          -> IO a            -- ^ Result returned by body
withPiLcd = withPiLcd' closePiLcd

-- | Like 'withPiLcd', but in the non-exceptional case, the display is
-- cleared and the backlight is turned off.
withPiLcdThenTurnOff :: LcdAddress
                     -> LcdOptions
                     -> (PiLcd -> IO a) -- ^ Body computation
                     -> IO a            -- ^ Result returned by body
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
