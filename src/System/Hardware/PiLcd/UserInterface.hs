module System.Hardware.PiLcd.UserInterface where

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

updateState :: UiState -> ButtonEvent -> UiState
updateState st (ButtonEvent ButtonSelect Press) = st { usInternal = DuringSelect }
updateState st (ButtonEvent ButtonSelect Release) = st { usInternal = AfterSelect }
updateState st (ButtonEvent ButtonUp    Press) = moveList    st (-1)
updateState st (ButtonEvent ButtonDown  Press) = moveList    st   1
updateState st (ButtonEvent ButtonLeft  Press) = moveButtons st (-1)
updateState st (ButtonEvent ButtonRight Press) = moveButtons st   1
updateState st _ = st

sanitizeState :: UiData -> UiState -> UiState
sanitizeState dat st =
  let nList    = length $ udList    dat
      nButtons = length $ udButtons dat
      sList    = (usList st)    `mod` nList
      sButtons = (usButtons st) `mod` nButtons
  in st { usList = sList, usButtons = sButtons }

renderUi :: UiData -> UiState -> Int -> [T.Text]
renderUi dat st columns =
  let lst = udList dat
      lstLine = case lst of
                  [] -> T.empty
                  [x] -> x
                  _ -> '↕' `T.cons` (lst !! (usList st))
      btnLine = expandButtons columns $ mkButtons (udButtons dat) (usButtons st)
  in [T.Take columns lstLine, scrollButtons columns btnLine]

mkButtons :: [T.Text] -> Int -> T.Text

expandButtons :: Int -> T.Text -> T.Text

scrollButtons :: Int -> T.Text -> T.Text

noAfter :: UiState -> UiState
noAfter st@(UiState { usInternal = AfterSelect }) =
  st { usInternal = BeforeSelect }
noAfter st = st
