#include <linux/i2c.h>
#include <linux/i2c-dev.h>

data Segment = Read Int | Write [Word8]

data I2cRdwrIoctlData =
  I2cRdwrIoctlData
  { i2c_msgs :: Ptr I2cMsg
  , i2c_nmsgs :: Word32
  }

instance Storable I2cRdwrIoctlData where
  sizeOf _ = #{size struct i2c_rdwr_ioctl_data}
  alignment _ = 4 -- having trouble getting #alignment to work
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
  alignment _ = 4 -- having trouble getting #alignment to work
  peek x = I2cMsg
           <$> #{peek struct i2c_msg, addr} x
           <*> #{peek struct i2c_msg, flags} x
           <*> #{peek struct i2c_msg, len} x
           <*> #{peek struct i2c_msg, buf} x
  poke p x = do
    #{poke struct i2c_msg, addr}  p (i2c_addr x)
    #{poke struct i2c_msg, flags} p (i2c_flags x)
    #{poke struct i2c_msg, len}   p (i2c_len x)
    #{poke struct i2c_msg, buf}   p (i2c_buf x)

flagRd :: Word16
flagRd = #const I2C_M_RD

segLength :: Segment -> Int
segLength (Read n) = n
segLength (Write xs) = length xs

i2cTransaction :: Fd -> Int -> [Segment] -> IO [[Word8]]
i2cTransaction (Fd fd) addr segs = do
  let len = sum $ map segLength segs
  allocaArray len $ \bytePtr -> do
    allocaArray (length segs) $ \segPtr -> do
      fillOutSegments segs segPtr bytePtr
      alloca $ \ioctlPtr -> do
        TODO
