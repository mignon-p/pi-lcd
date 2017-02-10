{-|
Module      : System.Hardware.PiLcd.UserInterace
Description : A simple user interface for LCDs
Copyright   : © Patrick Pelletier, 2017
License     : BSD3
Maintainer  : code@funwithsoftware.org

Displays a simple user interface.  The first line of the display
is used as a \"list box\", where the user can scroll through a
list of items one at a time using the up and down buttons.
The second line of the display is used for virtual \"buttons\",
such as \"OK\" and \"Cancel\".  The user uses the left and right buttons
to select a virtual \"button\".  When the user presses the
Select button, the interaction is considered done, and the calling
program is given the list item and button selection that the user
made.
-}

module System.Hardware.PiLcd.UserInterface
  ( -- * User interface
    UiData(..)
  , UiState(..)
  , InternalState
  , defaultUiState
  , runUi
    -- * Buttons
  , Button(..)
  , ButtonDirection(..)
  , ButtonEvent(..)
  ) where

import qualified Data.Text as T

import System.Hardware.PiLcd.Util

-- | Indicates one of the five buttons on the LCD+Keypad kit.
data Button = ButtonSelect | ButtonRight | ButtonDown | ButtonUp | ButtonLeft
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | Indicates whether a button was pressed or released.
data ButtonDirection = Press | Release
                     deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | Indicates a press or release of one of the five buttons on the
-- LCD+Keypad kit.
data ButtonEvent = ButtonEvent Button ButtonDirection
                 deriving (Eq, Ord, Show, Read)

-- | The data to be displayed in the user interface.
data UiData =
  UiData
  { udList    :: [T.Text] -- ^ items to be displayed one-at-a-time on
                          -- the first line
  , udButtons :: [T.Text] -- ^ buttons to be displayed on the second line
  } deriving (Eq, Ord, Show, Read)

-- | The current state of the user interaction.
data UiState =
  UiState
  { usList     :: Int  -- ^ index of item from 'udList' currently displayed
  , usButtons  :: Int  -- ^ index of button from 'udButtons' currently highlighted
  , usInternal :: InternalState -- ^ opaque data
  } deriving (Eq, Ord, Show)

-- | Opaque data.
data InternalState = BeforeSelect | DuringSelect | AfterSelect
  deriving (Eq, Ord)

instance Show InternalState where
  show _ = "<Internal State>"

-- | Defaults to displaying the first list item (index 0) and highlighting
-- the first button (index 0).
defaultUiState :: UiState
defaultUiState =
  UiState
  { usList = 0
  , usButtons = 0
  , usInternal = BeforeSelect
  }

-- | Computes the text which should be displayed for the current state of
-- the UI, and optionally applies a button press/release to the state.
runUi :: UiData             -- ^ Data to display in the UI
      -> UiState            -- ^ Current state of the interaction
      -> Maybe ButtonEvent  -- ^ optional button press/release
      -> Int                -- ^ Number of columns in the LCD
      -> ([T.Text], UiState, Bool) -- ^ text to display, new UI state,
                                   -- and a flag indicating whether the
                                   -- interaction is done (i. e. user has
                                   -- pressed and released the \"Select\"
                                   -- button)
runUi dat st mbe columns =
  let (st', done) = case mbe of
                      Nothing   -> (st, False)
                      (Just be) -> updateState st be
      st'' = sanitizeState dat st'
      ls = renderUi dat st'' columns
  in (ls, noAfter st'', done)

updateState :: UiState -> ButtonEvent -> (UiState, Bool)
updateState st (ButtonEvent ButtonSelect Press) = (st { usInternal = DuringSelect }, False)
updateState st (ButtonEvent ButtonSelect Release) = (st { usInternal = AfterSelect }, True)
updateState st (ButtonEvent ButtonUp    Press) = (moveList    st (-1), False)
updateState st (ButtonEvent ButtonDown  Press) = (moveList    st   1 , False)
updateState st (ButtonEvent ButtonLeft  Press) = (moveButtons st (-1), False)
updateState st (ButtonEvent ButtonRight Press) = (moveButtons st   1 , False)
updateState st _ = (st, False)

moveList :: UiState -> Int -> UiState
moveList st n = st { usList = usList st + n }

moveButtons :: UiState -> Int -> UiState
moveButtons st n = st { usButtons = usButtons st + n }

sanitizeState :: UiData -> UiState -> UiState
sanitizeState dat st =
  let nList    = length $ udList    dat
      nButtons = length $ udButtons dat
      sList    = usList st    `mod` nList
      sButtons = usButtons st `mod` nButtons
  in st { usList = sList, usButtons = sButtons }

renderUi :: UiData -> UiState -> Int -> [T.Text]
renderUi dat st columns =
  let lst = udList dat
      lstLine = case lst of
                  [] -> T.empty
                  [x] -> x
                  _ -> padLine (columns - 1) (lst !! usList st) `T.snoc` '↕'
      cc = arrows (usInternal st)
      btns = renderButtons cc (usButtons st) (mkButtons (udButtons dat) columns) 0
  in [lstLine, scrollButtons btns columns]

arrows :: InternalState -> (Char, Char)
arrows BeforeSelect = ('▶', '◀')
arrows DuringSelect = ('▷', '◁')
arrows AfterSelect  = (' ', ' ')

mkButtons :: [T.Text] -> Int -> [(Int, T.Text)]
mkButtons buttons columns =
  let haveColumns = sum $ map T.length buttons
      needColumns = columns - haveColumns
  in expandButtons (buttons ++ [T.empty]) needColumns

expandButtons :: [T.Text] -> Int -> [(Int, T.Text)]
expandButtons [] _ = []
expandButtons buttons@(bt:bts) needColumns =
  let nButtons = length buttons
      nSpaces = max 1 (needColumns `div` nButtons)
  in (nSpaces, bt) : expandButtons bts (needColumns - nSpaces)

renderButtons :: (Char, Char) -> Int -> [(Int, T.Text)] -> Int -> (Int, Int, T.Text)
renderButtons _ _ [] _ = (0, 0, T.empty)
renderButtons cs@(c1, c2) idx ((nSpaces, txt):bts) len =
  let l1 = len + nSpaces
      l2 = l1 + T.length txt
      (b, e, rest) = renderButtons cs (idx - 1) bts l2
      spaces = T.replicate (nSpaces - 1) (T.singleton ' ')
  in case idx of
       0    -> (l1, l2, T.concat [spaces, T.singleton c1,  txt, rest])
       (-1) -> (b,  e,  T.concat [T.singleton c2,  spaces, txt, rest])
       _    -> (b,  e,  T.concat [T.singleton ' ', spaces, txt, rest])

scrollButtons :: (Int, Int, T.Text) -> Int -> T.Text
scrollButtons (b, e, txt) columns =
  let mid = (b + e) `div` 2
      txtLen = T.length txt
      startCol = mid - (columns `div` 2)
      startCol' = max 0 $ min (txtLen - columns) startCol
  in T.take columns $ T.drop startCol' txt

noAfter :: UiState -> UiState
noAfter st@(UiState { usInternal = AfterSelect }) =
  st { usInternal = BeforeSelect }
noAfter st = st
