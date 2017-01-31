module Mcp23017
  ( ReadFunc
  , WriteFunc
  , PortExpander
  , mkPortExpander
  , writeIoDir
  , writeGpPu
  , writeGpio
  , readGpio
  , readGpioA
  , readGpioB
  ) where

import Control.Applicative
import Data.Bits
import Data.IORef
import Data.Word

ioDirA, ioDirB, iPolA, iPolB, gpIntEnA, gpIntEnB, defValA, defValB :: Word8
intConA, intConB, ioCon, gpPuA, gpPuB, intFA, intFB :: Word8
intCapA, intCapB, gpioA, gpioB, olatA, olatB :: Word8

-- We are going to assume/hope that the BANK bit in IOCON is 0
ioDirA   = 0
ioDirB   = 1
iPolA    = 2
iPolB    = 3
gpIntEnA = 4
gpIntEnB = 5
defValA  = 6
defValB  = 7
intConA  = 8
intConB  = 9
ioCon    = 10
gpPuA    = 12
gpPuB    = 13
intFA    = 14
intFB    = 15
intCapA  = 16
intCapB  = 17
gpioA    = 18
gpioB    = 19
olatA    = 20
olatB    = 21

type ReadFunc = Word8 -> Int -> IO [Word8]
type WriteFunc = Word8 -> [Word8] -> IO ()

data PortExpander =
  PortExpander
  { peRead  :: ReadFunc
  , peWrite :: WriteFunc
  , peIoDir :: IORef Word16
  , peGpPu  :: IORef Word16
  , peOlat  :: IORef Word16
  }

word8sToWord16 :: [Word8] -> Word16
word8sToWord16 [b1, b2] = (fromIntegral b1 `shiftL` 8) .|. fromIntegral b2

word16ToWord8s :: Word16 -> [Word8]
word16ToWord8s w = [fromIntegral (w `shiftR` 8), fromIntegral w]

readReg16 :: ReadFunc -> Word8 -> IO Word16
readReg16 rf reg = word8sToWord16 <$> rf reg 2

modifyReg16 :: WriteFunc -> IORef Word16 -> Word8 -> Word16 -> Word16 -> IO ()
modifyReg16 wf shadow reg bits mask = do
  old16 <- readIORef shadow
  let val = (old16 .&. complement mask) `xor` bits
      old8s@[o8a, o8b] = word16ToWord8s old16
      val8s@[v8a, v8b] = word16ToWord8s val
  case (o8a == v8a, o8b == v8b) of
    (False, False) -> wf reg val8s
    (False, True)  -> wf reg [v8a]
    (True, False)  -> wf (reg + 1) [v8b]
    (True, True)   -> return ()
  writeIORef shadow val

mkPortExpander :: ReadFunc -> WriteFunc -> IO PortExpander
mkPortExpander rf wf = do
  ioDir  <- readReg16 rf ioDirA
  gpPu   <- readReg16 rf gpPuA
  olat   <- readReg16 rf olatA
  rIoDir <- newIORef ioDir
  rGpPu  <- newIORef gpPu
  rOlat  <- newIORef olat
  -- clear SEQOP bit, to enable sequential operation
  [ioConOld] <- rf ioCon 1
  let ioConNew = clearBit ioConOld 5 -- 5 is SEQOP bit
  wf ioCon [ioConNew]
  return $ PortExpander
    { peRead = rf
    , peWrite = wf
    , peIoDir = rIoDir
    , peGpPu = rGpPu
    , peOlat = rOlat
    }

writeIoDir :: PortExpander -> Word16 -> Word16 -> IO ()
writeIoDir pe bits mask =
  modifyReg16 (peWrite pe) (peIoDir pe) ioDirA bits mask

writeGpPu :: PortExpander -> Word16 -> Word16 -> IO ()
writeGpPu pe bits mask =
  modifyReg16 (peWrite pe) (peGpPu pe) gpPuA bits mask

writeGpio :: PortExpander -> Word16 -> Word16 -> IO ()
writeGpio pe bits mask =
  modifyReg16 (peWrite pe) (peOlat pe) olatA bits mask

readGpio :: PortExpander -> IO Word16
readGpio pe = readReg16 (peRead pe) gpioA

readGpioA :: PortExpander -> IO Word8
readGpioA pe = do
  [r] <- peRead pe gpioA 1
  return r

readGpioB :: PortExpander -> IO Word8
readGpioB pe = do
  [r] <- peRead pe gpioB 1
  return r
