module LcdLowLevel where

data LcdBus =
  LcdBus
  { lbRS :: Bool
  , lbRW :: Bool
  , lbE  :: Bool
  , lbDB :: Maybe Word8 -- 4 LSB are DB4 to DB7; tri-state if Nothing
  } deriving (Eq, Ord, Show, Read)

instReg = False
dataReg = True

write = False
read  = True

nanoseconds :: Int -> Int
nanoseconds = id

microseconds :: Int -> Int
microseconds = (* 1000)

milliseconds :: Float -> Int
milliseconds x = ceil (x * 1e6)

data LcdCallbacks =
  LcdCallbacks
  { lcSend :: LcdBus -> IO ()
  , lcRecv :: IO Word8
  , lcDelay :: Int -> IO () -- delay in nanoseconds
  }

write4 :: LcdCallbacks -> Bool -> Word8 -> IO ()
write4 cb rs db = do
  let bus = LcdBus
            { lbRS = rs
            , lbRW = write
            , lbE = False
            , lbDB = Just db
            }
  lcSend cb $ bus { lbE = False }
  delayEnableLow1 cb
  lcSend cb $ bus { lbE = True }
  delayEnableHigh cb
  lcSend cb $ bus { lbE = False }
  delayEnableLow2 cb
  lcSend cb $ bus { ldDB = Nothing }

write8 :: LcdCallbacks -> Bool -> Word8 -> IO ()
write8 cb rs db = do
  write4 cb rs (db `shiftR` 4)
  write4 cb rs (db .&. 0xf)

read4 :: LcdCallbacks -> Bool -> IO Word8
read4 cb rs = do
  let bus = LcdBus
            { lbRS = rs
            , lbRW = read
            , lbE = False
            , lbDB = Nothing
            }
  lcSend cb $ bus { lbE = False }
  delayEnableLow1 cb
  lcSend cb $ bus { lbE = True }
  delayFoo cb
  d <- lcRecv cb
  delayBar cb
  lcSend cb $ bus { lbE = False }

read8 :: LcdCallbacks -> Bool -> IO Word8
read8 cb rs = do
  hi <- read4 cb rs
  lo <- read4 cb rs
  return $ (hi `shiftL` 4) .|. (lo .&. 0xf)

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
  write8 cb instReg 0x23 -- 2 display lines
  busyWait cb
  lcdOff cb
  lcdClear cb
  write8 cb instReg 0x04 -- TODO: I/D and S
  busyWait cb
  lcdOn cb

lcdOff :: LcdCallbacks -> IO ()
lcdOff cb = do
  write8 cb instReg displayOff
  busyWait cb

lcdOn :: LcdCallbacks -> IO ()
lcdOn cb = do
  write8 cb instReg displayOn
  busyWait cb
