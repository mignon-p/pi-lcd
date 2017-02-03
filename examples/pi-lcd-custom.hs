{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Data.Char

import System.Hardware.PiLcd

custom :: [(Char, [String])]
custom =
  [ ( chr 0xf800
    , [ "*    "
      , "**   "
      , "**   "
      , " **  "
      , " **  "
      , "  ** "
      , "  ** "
      , "   **"
      ]
    )
  , ( chr 0xf801
    , [ "     "
      , "     "
      , "     "
      , "     "
      , "     "
      , " ****"
      , " ****"
      , "     "
      ]
    )
  , ( chr 0xf802
    , [ "   **"
      , "  ** "
      , "  ** "
      , " **  "
      , " **  "
      , "**   "
      , "**   "
      , "*    "
      ]
    )
  , ( chr 0xf803
    , [ "  ***"
      , "* ***"
      , "*    "
      , "**   "
      , "**   "
      , " **  "
      , " **  "
      , "  ** "
      ]
    )
  ]

main = do
  let lcdOpts = defaultLcdOptions
                { loCustomChars = map (second charFromAsciiArt) custom }
  lcd <- openPiLcd defaultLcdAddress lcdOpts
  updateDisplay lcd [ "     \xf800\xf800\xf801"
                    , "     \xf802\xf802\xf803 Haskell"
                    ]
  setBacklightColor lcd Magenta
  closePiLcd lcd
