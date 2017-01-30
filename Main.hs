import Control.Monad
import Data.Bits
import Data.Word
import Text.Printf

import I2C
import PiLcd

printChanges :: I2cHandle -> Int -> Word8 -> IO ()
printChanges h addr old = do
  b' <- readGpioA h addr
  let b = (b' .&. 0x1f) `xor` 0x1f
  when (b /= old) $ putStrLn $ printf "%02x" b
  printChanges h addr b

main = do
  h <- i2cOpen 1
  printChanges h 0x20 0xff
  i2cClose h
