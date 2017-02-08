{-# LANGUAGE MultiWayIf #-}

module System.Hardware.PiLcd.UserInterface
  ( Button(..)
  , ButtonDirection(..)
  , ButtonEvent(..)
  , UiData(..)
  , UiState(..)
  , InternalState
  , defaultUiState
  , runUi
  ) where

import Data.Monoid
import qualified Data.Text as T

import System.Hardware.PiLcd.Font5x8

data Button = ButtonSelect | ButtonRight | ButtonDown | ButtonUp | ButtonLeft
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

data ButtonDirection = Press | Release
                     deriving (Eq, Ord, Show, Read, Bounded, Enum)

data ButtonEvent = ButtonEvent Button ButtonDirection
                 deriving (Eq, Ord, Show, Read)

data UiData =
  UiData
  { udList    :: [T.Text]
  , udButtons :: [T.Text]
  } deriving (Eq, Ord, Show, Read)

data UiState =
  UiState
  { usList     :: Int
  , usButtons  :: Int
  , usInternal :: InternalState
  } deriving (Eq, Ord, Show)

data InternalState = BeforeSelect | DuringSelect | AfterSelect
  deriving (Eq, Ord)

instance Show InternalState where
  show _ = "<Internal State>"

defaultUiState :: UiState
defaultUiState =
  UiState
  { usList = 0
  , usButtons = 0
  , usInternal = BeforeSelect
  }

runUi :: UiData
      -> UiState
      -> Maybe ButtonEvent
      -> Int
      -> ([T.Text], UiState, Bool)
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
      sList    = (usList st)    `mod` nList
      sButtons = (usButtons st) `mod` nButtons
  in st { usList = sList, usButtons = sButtons }

padLine :: Int -> T.Text -> T.Text
padLine columns txt =
  let len = T.length txt
  in if | len < columns -> txt <> T.replicate (columns - len) (T.singleton ' ')
        | len > columns -> T.take columns txt
        | otherwise -> txt

renderUi :: UiData -> UiState -> Int -> [T.Text]
renderUi dat st columns =
  let lst = udList dat
      lstLine = case lst of
                  [] -> T.empty
                  [x] -> x
                  _ -> padLine (columns - 1) (lst !! (usList st)) `T.snoc` '↕'
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
