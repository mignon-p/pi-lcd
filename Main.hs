import Control.Monad
import Data.Bits
import Data.Word
import Text.Printf

import I2C
import PiLcd

printChanges :: PiLcd -> Int -> Word8 -> Int -> IO ()
printChanges lcd addr old color = do
  b <- getButtons lcd
  color' <- if b == old
            then return color
            else do
              let nc = 7 .&. (color + 1)
                  nc' = toEnum nc
              setBacklightColor lcd nc'
              putStrLn $ printf "%02x %s" b (show nc')
              return nc
  printChanges lcd addr b color'

main = do
  h <- i2cOpen 1
  lcd <- mkPiLcd h
  printChanges lcd 0x20 0xff 0
  i2cClose h
