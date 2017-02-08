{-# LANGUAGE OverloadedStrings #-}

-- a demo of the user interface

import qualified Data.Text as T

import Control.Exception
import Data.Monoid
import System.Hardware.PiLcd

bandNames :: [T.Text]
bandNames =
  [ "Blue Öyster Cult"
  , "¡FФЯWДЯD, RUSSIД!"
  , "GL▲SS †33†H"
  , "Motörhead"
  , "Mötley Crüe"
  , "Queensrÿche"
  , "XYLØ"
  ]

uiData :: UiData
uiData =
  UiData
  { udList = bandNames
  , udButtons = ["OK", "Cancel"]
  }

main = do
  bracket (openPiLcd defaultLcdAddress $ defaultLcdOptions) closePiLcd
  $ \lcd -> do
    setBacklightColor lcd Blue
    st <- runUiUntilDone lcd uiData defaultUiState
    case usButtons st of
      0 -> do
        let band = bandNames !! usList st
        setBacklightColor lcd Green
        updateDisplay lcd ["You chose", "“" <> band <> "”"]
      1 -> do
        setBacklightColor lcd Red
        updateDisplay lcd ["You pressed", "“Cancel”"]
