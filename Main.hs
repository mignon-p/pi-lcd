import I2C

segs :: [Segment]
segs =
  [ Write [0]
  , Read 0x16
  ]

main = do
  h <- i2cOpen 1
  r <- i2cTransaction h 0x20 segs
  i2cClose h
  print r
