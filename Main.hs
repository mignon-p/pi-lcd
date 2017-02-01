{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Bits
import qualified Data.Text as T
import Data.Word
import Text.Printf

import Font5x8
import I2C
import PiLcd

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

main = do
  h <- i2cOpen 1
  lcd <- mkPiLcd h
  putStrLn "Hello, World!"
  forM_ "▶◀“”⏫⏬•⏎↑↓→←≤≥▲▼⌂♪♬♥" $ \c ->
    putStrLn $ unlines $ showCharacter c
  updateDisplay lcd ["¥→←∙∃□°αäβεμσρ√¢", "öΘΩüΣπ÷▮"]
  printChanges lcd 0x20 0
  i2cClose h
