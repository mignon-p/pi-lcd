module PiLcd
  ( readGpioA
  ) where

import Data.Word

import I2C

ioDirA, ioDirB, iPolA, iPolB, gpIntEnA, gpIntEnB, defValA, defValB :: Word8
intConA, intConB, ioCon, gpPuA, gpPuB, intFA, intFB :: Word8
intCapA, intCapB, gpioA, gpioB, olatA, olatB :: Word8

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

readGpioA :: I2cHandle -> Int -> IO Word8
readGpioA h addr = do
  [x] <- i2cReadReg h addr gpioA 1
  return x
