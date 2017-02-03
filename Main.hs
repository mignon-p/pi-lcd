{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Bits
import Data.Char
import Data.Monoid
import qualified Data.Text as T
import Data.Word
import Text.Printf

import System.Hardware.PiLcd

printChanges :: PiLcd -> Int -> Int -> IO ()
printChanges lcd addr color = do
  b <- getButtonEvent lcd
  color' <- case b of
              Nothing -> return color
              (Just btn@(ButtonEvent but dir)) -> do
                let nc = case dir of
                           Press -> 7 .&. (color + 1)
                           Release -> color
                    nc' = toEnum nc
                setBacklightColor lcd nc'
                let msg = [(drop 6 $ show but) ++ " " ++ (show dir), (show nc')]
                updateDisplay lcd $ map T.pack msg
                return nc
  printChanges lcd addr color'

myChar :: [String]
myChar =
  [ "*****"
  , "*  **"
  , "* * *"
  , "*  **"
  , "* ***"
  , "* ***"
  , "* ***"
  , "*****"
  ]

main = do
  let codePoint = chr 0xf800
      customChars = [(codePoint, charFromAsciiArt myChar)]
  lcd <- openPiLcd defaultLcdAddress $ defaultLcdOptions { loCustomChars = customChars }
  putStrLn "Hello, World!"
  updateDisplay lcd ["¥→←∙∃□°αäβεμσρ√¢", "öΘΩüΣπ÷▮≤≥▲▼⌂♪♬" <> T.singleton codePoint]
  printChanges lcd 0x20 0
