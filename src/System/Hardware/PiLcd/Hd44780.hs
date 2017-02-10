{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : System.Hardware.PiLcd.Hd44780
Description : Control the HD44780U LCD controller
Copyright   : © Patrick Pelletier, 2017
License     : BSD3
Maintainer  : code@funwithsoftware.org

This module lets you control an HD44780U Dot Matrix Liquid Crystal
Display Controller/Driver
(<https://www.adafruit.com/datasheets/HD44780.pdf datasheet>),
such as the one used in the
<https://www.adafruit.com/category/808 Adafruit LCD+Keypad Kit>.
-}

module System.Hardware.PiLcd.Hd44780
  ( LcdBus (..)
  , LcdCallbacks (..)
  , lcdInitialize
  , lcdClear
  , lcdControl
  , lcdWrite
  , lcdDefineChar
  ) where

import Control.Concurrent
import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import Data.Word

import System.Hardware.PiLcd.Util

data LcdBus =
  LcdBus
  { lbRS :: Bool
  , lbRW :: Bool
  , lbE  :: Bool
  , lbDB :: Maybe Word8 -- 4 LSB are DB4 to DB7; tri-state if Nothing
  } deriving (Eq, Ord, Show, Read)

instReg = False
dataReg = True

writeMode = False
readMode  = True

{-
nanoseconds :: Int -> Int
nanoseconds = id
-}

microseconds :: Int -> Int
microseconds = (* 1000)

milliseconds :: Float -> Int
milliseconds x = ceiling (x * 1e6)

enableCycle = 500
enableWidth = 230
addrSetup = 40
-- addrHold = 10
-- dataSetup = 80
-- dataHold = 10
dataDelay = 160

data LcdCallbacks =
  LcdCallbacks
  { lcSend :: LcdBus -> IO ()
  , lcRecv :: IO Word8
  , lcDelay :: Int -> IO () -- delay in nanoseconds
  }

delayEnableLow1 cb = lcDelay cb addrSetup
delayEnableHigh cb = lcDelay cb enableWidth
delayEnableLow2 cb = lcDelay cb (enableCycle - enableWidth - addrSetup)
delayEnableHigh1 cb = lcDelay cb dataDelay
delayEnableHigh2 cb = lcDelay cb (enableWidth - dataDelay)

write4 :: LcdCallbacks -> Bool -> Word8 -> IO ()
write4 cb rs db = do
  let bus = LcdBus
            { lbRS = rs
            , lbRW = writeMode
            , lbE = False
            , lbDB = Just db
            }
  lcSend cb $ bus { lbE = False }
  delayEnableLow1 cb
  lcSend cb $ bus { lbE = True }
  delayEnableHigh cb
  lcSend cb $ bus { lbE = False }
  delayEnableLow2 cb
  lcSend cb $ bus { lbDB = Nothing }

write8 :: LcdCallbacks -> Bool -> Word8 -> IO ()
write8 cb rs db = do
  write4 cb rs (db `shiftR` 4)
  write4 cb rs (db .&. 0xf)

read4 :: LcdCallbacks -> Bool -> IO Word8
read4 cb rs = do
  let bus = LcdBus
            { lbRS = rs
            , lbRW = readMode
            , lbE = False
            , lbDB = Nothing
            }
  lcSend cb $ bus { lbE = False }
  delayEnableLow1 cb
  lcSend cb $ bus { lbE = True }
  delayEnableHigh1 cb
  d <- lcRecv cb
  delayEnableHigh2 cb
  lcSend cb $ bus { lbE = False }
  delayEnableLow2 cb
  return d

read8 :: LcdCallbacks -> Bool -> IO Word8
read8 cb rs = do
  hi <- read4 cb rs
  lo <- read4 cb rs
  return $ (hi `shiftL` 4) .|. (lo .&. 0xf)

-- | Initializes the LCD and clears the screen.  You must
-- call this function first, before any of the other
-- LCD functions.
lcdInitialize :: LcdCallbacks -> IO ()
lcdInitialize cb = do
  -- initialization according to Figure 24
  write4 cb instReg 3
  lcDelay cb $ milliseconds 4.1
  write4 cb instReg 3
  lcDelay cb $ microseconds 100
  write4 cb instReg 3
  write4 cb instReg 2
  busyWait cb
  doCmd cb 0x28 -- 2 display lines
  lcdControl cb False False False -- display off
  lcdClear cb
  lcdMode cb True False -- left-to-right, no scrolling
  lcdControl cb True False False -- display on

doCmd :: LcdCallbacks -> Word8 -> IO ()
doCmd cb cmd = do
  write8 cb instReg cmd
  busyWait cb

doData :: LcdCallbacks -> Word8 -> IO ()
doData cb cmd = do
  write8 cb dataReg cmd
  busyWait cb

busyWait :: LcdCallbacks -> IO ()
busyWait cb = do
  bfac <- read8 cb instReg
  when (testBit bfac 7) $ do
    yield
    busyWait cb

-- | Clears the screen.
lcdClear :: LcdCallbacks -> IO ()
lcdClear cb = doCmd cb (bit 0)

{-
lcdHome :: LcdCallbacks -> IO ()
lcdHome cb = doCmd cb (bit 1)
-}

-- | Controls what is shown on the LCD.
lcdControl :: LcdCallbacks
           -> Bool -- ^ Enable display
           -> Bool -- ^ Enable underline cursor
           -> Bool -- ^ Enable blinking block cursor
           -> IO ()
lcdControl cb d c b =
  doCmd cb (bit 3 +
            bitIf d 2 +
            bitIf c 1 +
            bitIf b 0)

lcdMode :: LcdCallbacks -> Bool -> Bool -> IO ()
lcdMode cb id s =
  doCmd cb (bit 2 +
            bitIf id 1 +
            bitIf s 0)

-- | Writes text onto the screen.  To position the cursor
-- without writing anything, you can write a zero-length string.
lcdWrite :: LcdCallbacks
         -> Word8 -- ^ Line number.  Must be 0 or 1.  (Even
                  -- <https://www.adafruit.com/products/198 displays with four physical lines>
                  -- are modeled internally as 2 lines.)
         -> Word8 -- ^ Column number, starting at 0.
         -> B.ByteString -- ^ Characters to write to the display.  Must be
                         -- encoded in the controller's native character
                         -- encoding.  See Table 4 on pages 17-18 of the
                         -- HD44780U datasheet.
         -> IO ()
lcdWrite cb line col bs = do
  let pos = col + line * 0x40
  doCmd cb (0x80 .|. pos)
  forM_ (B.unpack bs) $ \b -> doData cb b

{-
lcdRead :: LcdCallbacks -> Word8 -> Word8 -> Word8 -> IO B.ByteString
lcdRead cb line col len = do
  let pos = col + line * 0x40
  doCmd cb (0x80 .|. pos)
  ws <- forM [0..len-1] $ \_ -> do
    d <- read8 cb dataReg
    busyWait cb
    return d
  return $ B.pack ws
-}

-- | Defines a custom character.
lcdDefineChar :: LcdCallbacks
              -> Word8   -- ^ The character code to define.  Must be 0-7.
              -> [Word8] -- ^ The bitmap data.  Must be 8 bytes, with
                         -- data in the least significant 5 bits of each byte.
              -> IO ()
lcdDefineChar cb c bitmap = do
  when (c >= 8) $
    fail $ "lcdDefineChar: character must be between 0-7; got " ++ show c
  let len = length bitmap
  when (len /= 8) $
    fail $ "lcdDefineChar: bitmap must have 8 elements; got " ++ show len
  let pos = c * 8
  doCmd cb (0x40 .|. pos)
  forM_ bitmap $ \b -> doData cb b
