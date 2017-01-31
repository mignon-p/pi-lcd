import Control.Monad
import Data.Bits
import Data.Word
import Text.Printf

import I2C
import PiLcd

printChanges :: PiLcd -> Int -> Word8 -> IO ()
printChanges lcd addr old = do
  b' <- getButtons lcd
  let b = (b' .&. 0x1f) `xor` 0x1f
  when (b /= old) $ putStrLn $ printf "%02x" b
  printChanges lcd addr b

main = do
  h <- i2cOpen 1
  lcd <- mkPiLcd h
  printChanges lcd 0x20 0xff
  i2cClose h
