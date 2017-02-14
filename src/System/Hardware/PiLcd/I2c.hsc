{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : System.Hardware.PiLcd.I2c
Description : Access peripherals via an I²C bus
Copyright   : © Patrick Pelletier, 2017
License     : BSD3
Maintainer  : code@funwithsoftware.org
Portability : Linux

You can use this module to communicate with
<https://en.wikipedia.org/wiki/I%C2%B2C I²C> peripherals on Linux.
-}

module System.Hardware.PiLcd.I2c
  ( I2cHandle
  , Segment (..)
  , i2cOpen
  , i2cTransaction
  , i2cReadReg
  , i2cWriteReg
  , i2cClose
  ) where

import Control.Applicative
import Control.Monad
import Data.Word
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Posix.IO
import System.Posix.Types

#include <linux/i2c.h>
#include <linux/i2c-dev.h>

foreign import ccall "sys/ioctl.h ioctl" c_ioctl ::
  CInt -> CULong -> Ptr I2cRdwrIoctlData -> IO CInt

-- | Represents an I²C bus.
type I2cHandle = Fd

-- | Represents an I²C read or write, as part of a larger transaction.
data Segment = Read Int      -- ^ number of bytes to read
             | Write [Word8] -- ^ bytes to write

data I2cRdwrIoctlData =
  I2cRdwrIoctlData
  { i2c_msgs :: Ptr I2cMsg
  , i2c_nmsgs :: Word32
  }

instance Storable I2cRdwrIoctlData where
  sizeOf _ = #{size struct i2c_rdwr_ioctl_data}
  alignment x = alignment (i2c_nmsgs x)
  peek x = I2cRdwrIoctlData
           <$> #{peek struct i2c_rdwr_ioctl_data, msgs}  x
           <*> #{peek struct i2c_rdwr_ioctl_data, nmsgs} x
  poke p x = do
    #{poke struct i2c_rdwr_ioctl_data, msgs}  p (i2c_msgs x)
    #{poke struct i2c_rdwr_ioctl_data, nmsgs} p (i2c_nmsgs x)

data I2cMsg =
  I2cMsg
  { i2c_addr  :: Word16
  , i2c_flags :: Word16
  , i2c_len   :: Word16
  , i2c_buf   :: Ptr Word8
  }

instance Storable I2cMsg where
  sizeOf _ = #{size struct i2c_msg}
  alignment x = alignment (i2c_buf x)
  peek x = I2cMsg
           <$> #{peek struct i2c_msg, addr}  x
           <*> #{peek struct i2c_msg, flags} x
           <*> #{peek struct i2c_msg, len}   x
           <*> #{peek struct i2c_msg, buf}   x
  poke p x = do
    #{poke struct i2c_msg, addr}  p (i2c_addr x)
    #{poke struct i2c_msg, flags} p (i2c_flags x)
    #{poke struct i2c_msg, len}   p (i2c_len x)
    #{poke struct i2c_msg, buf}   p (i2c_buf x)

flagRd :: Word16
flagRd = #const I2C_M_RD

i2cRdwr :: CULong
i2cRdwr = #const I2C_RDWR

segLength :: Segment -> Int
segLength (Read n) = n
segLength (Write xs) = length xs

fillOutSegments :: Int -> [Segment] -> Ptr I2cMsg -> Ptr Word8 -> IO ()
fillOutSegments _ [] _ _ = return ()
fillOutSegments addr (seg:segs) segPtr bytePtr = do
  let len = segLength seg
      msg = I2cMsg
            { i2c_addr = fromIntegral addr
            , i2c_flags = case seg of
                            (Read _) -> flagRd
                            (Write _) -> 0
            , i2c_len = fromIntegral len
            , i2c_buf = bytePtr
            }
  poke segPtr msg
  case seg of
    (Read _) -> return ()
    (Write x) -> pokeArray bytePtr x
  fillOutSegments addr segs (advancePtr segPtr 1) (advancePtr bytePtr len)

collectResults :: [Segment] -> Ptr Word8 -> IO [[Word8]]
collectResults [] _ = return []
collectResults ((Read n):segs) bytePtr = do
  xs <- peekArray n bytePtr
  rest <- collectResults segs (advancePtr bytePtr n)
  return $ xs : rest
collectResults ((Write xs):segs) bytePtr =
  collectResults segs $ advancePtr bytePtr $ length xs

-- | Performs an
-- <https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/tree/Documentation/i2c/dev-interface I2C_RDWR>
-- transaction.
i2cTransaction :: I2cHandle    -- ^ handle to an I2C bus
               -> Int          -- ^ address of device on I2C bus
               -> [Segment]    -- ^ list of read/write segments to perform
               -> IO [[Word8]] -- ^ list of bytes returned for each read segment
i2cTransaction (Fd fd) addr segs = do
  let len = sum $ map segLength segs
      nSegs = length segs
  allocaArray len $ \bytePtr -> do
    allocaArray nSegs $ \segPtr -> do
      fillOutSegments addr segs segPtr bytePtr
      alloca $ \ioctlPtr -> do
        poke ioctlPtr $ I2cRdwrIoctlData segPtr $ fromIntegral nSegs
        r <- c_ioctl fd i2cRdwr ioctlPtr
        when (r < 0) $ throwErrno "i2cTransaction"
        collectResults segs bytePtr

-- | Open a handle to an I²C bus.
i2cOpen :: Int -- ^ bus number
        -> IO I2cHandle
i2cOpen bus = do
  let name = "/dev/i2c-" ++ show bus
  openFd name ReadWrite Nothing defaultFileFlags

-- | Close a handle to an I²C bus.
i2cClose :: I2cHandle -> IO ()
i2cClose = closeFd

-- | Writes the register number, and then reads the specified
-- number of bytes.
i2cReadReg :: I2cHandle  -- ^ handle to an I²C bus
           -> Int        -- ^ address of device on the I²C bus
           -> Word8      -- ^ register number
           -> Int        -- ^ number of bytes to read
           -> IO [Word8]
i2cReadReg h addr reg len = do
  [r] <- i2cTransaction h addr [Write [reg], Read len]
  return r

-- | Writes the register number, and then writes the specified bytes.
i2cWriteReg :: I2cHandle  -- ^ handle to an I²C bus
            -> Int        -- ^ address of device on the I²C bus
            -> Word8      -- ^ register number
            -> [Word8]    -- ^ bytes to write
            -> IO ()
i2cWriteReg h addr reg d = do
  i2cTransaction h addr [Write (reg:d)]
  return ()
