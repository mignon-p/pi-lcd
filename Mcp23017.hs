module Mcp23017 where

type ReadFunc = Word8 -> Int -> IO [Word8]
type WriteFunc = Word8 -> [Word8] -> IO ()

data PortExpander =
  PortExpander
  { peRead  :: ReadFunc
  , peWrite :: WriteFunc
  , peIoDir :: IORef Word16
  , peOlat  :: IORef Word16
  }

word8sToWord16 :: [Word8] -> Word16
word8sToWord16 [b1, b2] = (fromIntegral b1 `shiftL` 8) .|. fromIntegral b2

word16ToWord8s :: Word16 -> [Word8]
word16ToWord8s w = [fromIntegral (w `shiftR` 8), fromIntegral w]

readReg16 :: ReadFunc -> Word8 -> IO Word16
readReg16 rf reg = word8sToWord16 <$> rf reg 2

writeIfChanged16 :: WriteFunc -> IORef Word16 -> Word8 -> Word16 -> IO ()
writeIfChanged16 wf shadow reg val = do
  old16 <- readIORef shadow
  let old8s@[o8a, o8b] = word8sToWord16 old16
      val8s@[v8a, v8b] = word8sToWord16 val
  case (o8a == v8a, o8b == v8b) of
    (False, False) -> wf reg val8s
    (False, True)  -> wf reg [v8a]
    (True, False)  -> wf (reg + 1) [v8b]
    (True, True)   -> return ()
  writeIORef shadow val

mkPortExpander :: ReadFunc -> WriteFunc -> IO PortExpander
mkPortExpander rf wf = do
  ioDir <- readReg16 rf ioDirA
  olat <- readReg16 rf olatA
  rIoDir <- newIORef ioDir
  rOlat <- newIORef olat
  return $ PortExpander
    { peRead = rf
    , peWrite = wf
    , peIoDir = rIoDir
    , peOlat = rOlat
    }

