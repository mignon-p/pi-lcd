{-|
Module      : System.Hardware.PiLcd.Mcp23017
Description : Control the MCP23017 port expander
Copyright   : © Patrick Pelletier, 2017
License     : BSD3
Maintainer  : code@funwithsoftware.org

This module lets you control an
<https://www.adafruit.com/products/732 MCP23017> port expander
(<https://cdn-shop.adafruit.com/datasheets/mcp23017.pdf datasheet>),
such as the one used in the
<https://www.adafruit.com/category/808 Adafruit LCD+Keypad Kit>.

'PortExpander' is not threadsafe (you'll need to do your own locking),
but it is safe in the presence of async exceptions.
-}

module System.Hardware.PiLcd.Mcp23017
  ( -- * Creating a Port Expander
    -- | Note: the 'PortExpander' caches register values, so it
    -- assumes no one else is going to be writing to the MCP23017
    -- during the lifetime of the 'PortExpander'.
    PortExpander
  , mkPortExpander
    -- ** Read and write functions
    -- | These functions are used for reading and writing the
    -- MCP23017\'s registers.  Generally, you would want to specify
    -- the @i2cReadReg@ and @i2cWriteReg@ functions from the
    -- @System.Hardware.PiLcd.I2c@ module, partially applied to
    -- the @I2cHandle@ and the chip's address on the I²C bus.
    -- However, by supplying SPI read and write functions instead,
    -- you could probably interface with an MCP23S17, although
    -- this has not been tested.
  , ReadFunc
  , WriteFunc
    -- * Writing to registers
    -- | Each pair of 8-bit registers (\"A\" and \"B\") is treated
    -- as a 16-bit register, with \"A\" in the most significant
    -- 8 bits, and \"B\" in the least significant 8 bits.
    -- Each write function takes a mask operand, which specifies
    -- which bits to change.  Bits which are \"0\" in the mask
    -- are left unchanged.
  , writeIoDir
  , writeIPol
  , writeGpPu
  , writeGpio
    -- * Reading from registers
  , readGpio
  , readGpioA
  , readGpioB
  ) where

import Control.Applicative
import Control.Exception
import Data.Bits
import Data.IORef
import Data.Word

import System.Hardware.PiLcd.Util

ioDirA, iPolA, ioCon, gpPuA, gpioA, gpioB, olatA :: Word8

-- We are going to assume/hope that the BANK bit in IOCON is 0
ioDirA   = 0
-- ioDirB   = 1
iPolA    = 2
-- iPolB    = 3
-- gpIntEnA = 4
-- gpIntEnB = 5
-- defValA  = 6
-- defValB  = 7
-- intConA  = 8
-- intConB  = 9
ioCon    = 10
gpPuA    = 12
-- gpPuB    = 13
-- intFA    = 14
-- intFB    = 15
-- intCapA  = 16
-- intCapB  = 17
gpioA    = 18
gpioB    = 19
olatA    = 20
-- olatB    = 21

-- | Specifies a register number, and the number of
-- consecutive bytes to read.
type ReadFunc = Word8 -> Int -> IO [Word8]

-- | Specifies the register number, and the bytes to write.
type WriteFunc = Word8 -> [Word8] -> IO ()

-- | Opaque type representing an MCP23017 port expander.
data PortExpander =
  PortExpander
  { peRead  :: ReadFunc
  , peWrite :: WriteFunc
  , peIoDir :: IORef Word16
  , peIPol  :: IORef Word16
  , peGpPu  :: IORef Word16
  , peOlat  :: IORef Word16
  }

readReg16 :: ReadFunc -> Word8 -> IO Word16
readReg16 rf reg = word8sToWord16 <$> rf reg 2

modifyReg16 :: WriteFunc -> IORef Word16 -> Word8 -> Word16 -> Word16 -> IO ()
modifyReg16 wf shadow reg bits mask = mask_ $ do
  old16 <- readIORef shadow
  let val = (old16 .&. complement mask) .|. (bits .&. mask)
      [o8a, o8b]       = word16ToWord8s old16
      val8s@[v8a, v8b] = word16ToWord8s val
  case (o8a == v8a, o8b == v8b) of
    (False, False) -> wf reg val8s
    (False, True)  -> wf reg [v8a]
    (True, False)  -> wf (reg + 1) [v8b]
    (True, True)   -> return ()
  writeIORef shadow val

-- | Given a 'ReadFunc' and a 'WriteFunc', makes a 'PortExpander'.
mkPortExpander :: ReadFunc -> WriteFunc -> IO PortExpander
mkPortExpander rf wf = do
  ioDir  <- readReg16 rf ioDirA
  iPol   <- readReg16 rf iPolA
  gpPu   <- readReg16 rf gpPuA
  olat   <- readReg16 rf olatA
  rIoDir <- newIORef ioDir
  rIPol  <- newIORef iPol
  rGpPu  <- newIORef gpPu
  rOlat  <- newIORef olat
  -- clear SEQOP bit, to enable sequential operation
  [ioConOld] <- rf ioCon 1
  let ioConNew = clearBit ioConOld 5 -- 5 is SEQOP bit
  wf ioCon [ioConNew]
  return PortExpander
    { peRead = rf
    , peWrite = wf
    , peIoDir = rIoDir
    , peIPol = rIPol
    , peGpPu = rGpPu
    , peOlat = rOlat
    }

-- | Write to I/O Direction Register.  (§ 1.6.1 of datasheet)
writeIoDir :: PortExpander
           -> Word16 -- ^ The value to write.  A \'1\' bit indicates an
                     -- input, and a \'0\' bit indicates an output.
           -> Word16 -- ^ Mask of bits to write.
           -> IO ()
writeIoDir pe bits mask =
  modifyReg16 (peWrite pe) (peIoDir pe) ioDirA bits mask

-- | Write to Input Polarity Register.  (§ 1.6.2 of datasheet)
writeIPol :: PortExpander
          -> Word16 -- ^ The value to write.  A \'1\' bit means the input
                    -- will be inverted.
          -> Word16 -- ^ Mask of bits to write.
          -> IO ()
writeIPol pe bits mask =
  modifyReg16 (peWrite pe) (peIPol pe) iPolA bits mask

-- | Write to Pull-Up Resistor Configuration Register.  (§ 1.6.7 of datasheet)
writeGpPu :: PortExpander
          -> Word16 -- ^ The value to write.  A \'1\' bit means the input
                    -- will be pulled up with a 100 kΩ resistor.
          -> Word16 -- ^ Mask of bits to write.
          -> IO ()
writeGpPu pe bits mask =
  modifyReg16 (peWrite pe) (peGpPu pe) gpPuA bits mask

-- | Write to Output Latch Register.  (§ 1.6.11 of datasheet)
writeGpio :: PortExpander
          -> Word16 -- ^ The value to write.  This controls the value
                    -- of the output pins.
          -> Word16 -- ^ Mask of bits to write.
          -> IO ()
writeGpio pe bits mask =
  modifyReg16 (peWrite pe) (peOlat pe) olatA bits mask

-- | Read from the Port Register.  (§ 1.6.10 of datasheet)
-- Port A is in the most significant 8 bits, and Port B is in the
-- least significant 8 bits.
readGpio :: PortExpander -> IO Word16
readGpio pe = readReg16 (peRead pe) gpioA

-- | Read from Port Register A.  (§ 1.6.10 of datasheet)
readGpioA :: PortExpander -> IO Word8
readGpioA pe = do
  [r] <- peRead pe gpioA 1
  return r

-- | Read from Port Register B.  (§ 1.6.10 of datasheet)
readGpioB :: PortExpander -> IO Word8
readGpioB pe = do
  [r] <- peRead pe gpioB 1
  return r
