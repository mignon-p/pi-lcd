module UnicodeLcd
  ( Lcd
  , supportedChars
  , mkLcd
  , updateDisplay
  ) where

import Control.Monad
import Data.Char
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Data.Word

import LcdLowLevel

data Lcd =
  Lcd
  { lcdCb :: LcdCallbacks
  , lcdLines :: IORef [B.ByteString]
  , lcdCustom :: IORef CustomInfo
  }

type CustomInfo = (Integer, [(Char, Integer)])

-- https://forums.adafruit.com/viewtopic.php?f=50&t=111019

{- sadly, this is the table for ROM A02, which we don't have :(
table :: [(Int, Word8)]
table =
  [ (0x25B6, 0x10)  -- ▶ BLACK RIGHT-POINTING TRIANGLE
  , (0x25C0, 0x11)  -- ◀ BLACK LEFT-POINTING TRIANGLE
  , (0x201c, 0x12)  -- “ LEFT DOUBLE QUOTATION MARK
  , (0x201d, 0x13)  -- ” RIGHT DOUBLE QUOTATION MARK
  , (0x23EB, 0x14)  -- ⏫ BLACK UP-POINTING DOUBLE TRIANGLE
  , (0x23EC, 0x15)  -- ⏬ BLACK DOWN-POINTING DOUBLE TRIANGLE
  , (0x2022, 0x16)  -- • BULLET
  , (0x23CE, 0x17)  -- ⏎ RETURN SYMBOL
  , (0x2191, 0x18)  -- ↑ UPWARDS ARROW
  , (0x2193, 0x19)  -- ↓ DOWNWARDS ARROW
  , (0x2192, 0x1a)  -- → RIGHTWARDS ARROW
  , (0x2190, 0x1b)  -- ← LEFTWARDS ARROW
  , (0x2264, 0x1c)  -- ≤ LESS-THAN OR EQUAL TO
  , (0x2265, 0x1d)  -- ≥ GREATER-THAN OR EQUAL TO
  , (0x25B2, 0x1e)  -- ▲ BLACK UP-POINTING TRIANGLE
  , (0x25BC, 0x1f)  -- ▼ BLACK DOWN-POINTING TRIANGLE
  , (0x2302, 0x7f)  -- ⌂ HOUSE
  , (0x0411, 0x80)  -- Б CYRILLIC CAPITAL LETTER BE
  , (0x0414, 0x81)  -- Д CYRILLIC CAPITAL LETTER DE
  , (0x0416, 0x82)  -- Ж CYRILLIC CAPITAL LETTER ZHE
  , (0x0417, 0x83)  -- З CYRILLIC CAPITAL LETTER ZE
  , (0x0418, 0x84)  -- И CYRILLIC CAPITAL LETTER I
  , (0x0419, 0x85)  -- Й CYRILLIC CAPITAL LETTER SHORT I
  , (0x041B, 0x86)  -- Л CYRILLIC CAPITAL LETTER EL
  , (0x041F, 0x87)  -- П CYRILLIC CAPITAL LETTER PE
  , (0x0423, 0x88)  -- У CYRILLIC CAPITAL LETTER U
  , (0x0426, 0x89)  -- Ц CYRILLIC CAPITAL LETTER TSE
  , (0x0427, 0x8a)  -- Ч CYRILLIC CAPITAL LETTER CHE
  , (0x0428, 0x8b)  -- Ш CYRILLIC CAPITAL LETTER SHA
  , (0x0429, 0x8c)  -- Щ CYRILLIC CAPITAL LETTER SHCHA
  , (0x042A, 0x8d)  -- Ъ CYRILLIC CAPITAL LETTER HARD SIGN
  , (0x042B, 0x8e)  -- Ы CYRILLIC CAPITAL LETTER YERU
  , (0x042D, 0x8f)  -- Э CYRILLIC CAPITAL LETTER E
  , (0x03B1, 0x90)  -- α GREEK SMALL LETTER ALPHA
  , (0x266A, 0x91)  -- ♪ EIGHTH NOTE
  , (0x0393, 0x92)  -- Γ GREEK CAPITAL LETTER GAMMA
  , (0x03C0, 0x93)  -- π GREEK SMALL LETTER PI
  , (0x03A3, 0x94)  -- Σ GREEK CAPITAL LETTER SIGMA
  , (0x03C3, 0x95)  -- σ GREEK SMALL LETTER SIGMA
  , (0x266C, 0x96)  -- ♬ BEAMED SIXTEENTH NOTES
  , (0x03C4, 0x97)  -- τ GREEK SMALL LETTER TAU
  , (0x1F514, 0x98) -- 🔔 BELL
  , (0x0398, 0x99)  -- Θ GREEK CAPITAL LETTER THETA
  , (0x03A9, 0x9a)  -- Ω GREEK CAPITAL LETTER OMEGA
  , (0x03B4, 0x9b)  -- δ GREEK SMALL LETTER DELTA
  -- can't tell what 0x9c is supposed to be
  , (0x2665, 0x9d)  -- ♥ BLACK HEART SUIT
  , (0x03B5, 0x9e)  -- ε GREEK SMALL LETTER EPSILON
  , (0x2229, 0x9f)  -- ∩ INTERSECTION
  , (0x2016, 0xa0)  -- ‖ DOUBLE VERTICAL LINE
  ]

identityChars :: [Word8]
identityChars = [0x20..0x7e] ++ [0xa1..0xff]
-}

-- table for ROM A00, which is much more limited than A02
table :: [(Int, Word8)]
table =
  [ (0x00A5, 0x5c)  -- ¥ YEN SIGN
  , (0x2192, 0x7e)  -- → RIGHTWARDS ARROW
  , (0x2190, 0x7f)  -- ← LEFTWARDS ARROW
  , (0x2219, 0xa5)  -- ∙ BULLET OPERATOR
  , (0x2203, 0xd6)  -- ∃ THERE EXISTS
  , (0x25A1, 0xdb)  -- □ WHITE SQUARE
  , (0x00B0, 0xdf)  -- ° DEGREE SIGN
  , (0x03B1, 0xe0)  -- α GREEK SMALL LETTER ALPHA
  , (0x00E4, 0xe1)  -- ä LATIN SMALL LETTER A WITH DIAERESIS
  , (0x03B2, 0xe2)  -- β GREEK SMALL LETTER BETA
  , (0x03B5, 0xe3)  -- ε GREEK SMALL LETTER EPSILON
  , (0x03BC, 0xe4)  -- μ GREEK SMALL LETTER MU
  , (0x03C3, 0xe5)  -- σ GREEK SMALL LETTER SIGMA
  , (0x03C1, 0xe6)  -- ρ GREEK SMALL LETTER RHO
  , (0x221A, 0xe8)  -- √ SQUARE ROOT
  , (0x00A2, 0xec)  -- ¢ CENT SIGN
  , (0x00F6, 0xef)  -- ö LATIN SMALL LETTER O WITH DIAERESIS
  , (0x0398, 0xf2)  -- Θ GREEK CAPITAL LETTER THETA
  , (0x03A9, 0xf4)  -- Ω GREEK CAPITAL LETTER OMEGA
  , (0x00FC, 0xf5)  -- ü LATIN SMALL LETTER U WITH DIAERESIS
  , (0x03A3, 0xf6)  -- Σ GREEK CAPITAL LETTER SIGMA
  , (0x03C0, 0xf7)  -- π GREEK SMALL LETTER PI
  , (0x00F7, 0xfd)  -- ÷ DIVISION SIGN
  , (0x25AE, 0xff)  -- ▮ BLACK VERTICAL RECTANGLE
  ]

identityChars :: [Word8]
identityChars = [0x20..0x5b] ++ [0x5d..0x7d]

fullTable :: [(Int, Word8)]
fullTable = table ++ map f identityChars
  where f c = (fromIntegral c, c)

hashTable :: H.HashMap Int Word8
hashTable = H.fromList fullTable

supportedChars :: [Char]
supportedChars =
  map chr $ sort $ map fst fullTable

unicodeToByte :: Int -> Maybe Word8
unicodeToByte c = H.lookup c hashTable

ff :: (Int, [(Int, Int)]) -> [Bool] -> (Int, [(Int, Int)])
ff (len, spans) bools =
  let myLen = length bools
      polarity = head bools
      spans' = if polarity
               then spans -- new bytes and old bytes are equal
               else (len, myLen) : spans
      len' = len + myLen
  in (len', spans')

extractBytes :: B.ByteString -> (Int, Int) -> (Int, B.ByteString)
extractBytes bs (col, len) = (col, subStr)
  where subStr = B.take len $ B.drop col bs

findSpans :: B.ByteString -> B.ByteString -> [(Int, B.ByteString)]
findSpans old new =
  let bitMap = zipWith (==) (B.unpack old) (B.unpack new)
      grp = group bitMap
      pairs = snd $ foldl' ff (0, []) grp
  in map (extractBytes new) pairs

addLine :: [(Int, B.ByteString)] -> Int -> [(Int, Int, B.ByteString)]
addLine spans line = map f spans
  where f (col, bs) = (line, col, bs)

bytesToSpans :: [B.ByteString] -> [B.ByteString] -> [(Int, Int, B.ByteString)]
bytesToSpans old new =
  let spans = zipWith findSpans old new
      spans' = zipWith addLine spans [0..]
  in concat spans'

numColumns = 16
numLines = 2

ensureLength :: [T.Text] -> [T.Text]
ensureLength ls = map ensureCols $ take numLines $ ls ++ repeat T.empty
  where
    ensureCols t =
      T.take numColumns $ T.append t $ T.replicate numColumns $ T.singleton ' '

txtToBs :: T.Text -> B.ByteString
txtToBs txt = B.pack $ map (fromMaybe 0x3f . unicodeToByte . ord) $ T.unpack txt

updateDisplay :: Lcd -> [T.Text] -> IO ()
updateDisplay lcd newTxt = do
  oldBs <- readIORef (lcdLines lcd)
  let newTxt' = ensureLength newTxt
      newBs = map txtToBs newTxt'
      spans = bytesToSpans oldBs newBs
  forM_ spans $ \(line, col, bs) ->
    lcdWrite (lcdCb lcd) (fromIntegral line) (fromIntegral col) bs
  writeIORef (lcdLines lcd) newBs

mkLcd :: LcdCallbacks -> IO Lcd
mkLcd cb = do
  let ls = replicate numLines $ B.replicate numColumns 0x20
      nonChar = chr 0xffff -- a noncharacter according to Unicode standard
  ref <- newIORef ls
  cust <- newIORef (0, replicate 8 (nonChar, 0))
  return $ Lcd cb ref cust

data CharStatus = CharBuiltin | CharCustom | CharNotFound
                deriving (Eq, Ord, Show, Read, Bounded, Enum)

getCharStatus :: Lcd -> Char -> CharStatus
getCharStatus = undefined

getCharData :: Lcd -> Char -> Maybe [Word8]
getCharData = undefined

getCustomChars :: Lcd -> String -> String
getCustomChars lcd str =
  sort $ nub $ filter (\x -> getCharStatus lcd x == CharCustom) str

matchExistingChars :: [(Char, Integer)] -> String -> (String, [(Int, Integer)])
matchExistingChars cust chars =
  let pairs = zip [0..] cust
      existing = map fst cust
      common = existing `intersect` chars
      chars' = chars \\ common
      pairs' = filter ff pairs
      ff (_, (c, _)) = c `notElem` common
      f (pos, (_, generation)) = (pos, generation)
  in (chars', map f pairs')

allocateCustomChars :: CustomInfo -> String -> CustomInfo
allocateCustomChars ci chars =
  let (chars', available) = matchExistingChars (snd ci) chars
      available' = map fst $ sortBy (comparing snd) available
      pairs = zip available' chars'
      generation = 1 + (fst ci)
      newStuff = zipWith replace [0..] (snd ci)
      replace i old@(c, _) = case i `lookup` pairs of
                               Nothing -> if c `elem` chars
                                          then (c, generation)
                                          else old
                               (Just c') -> (c', generation)
  in (generation, newStuff)

writeCustomChars :: Lcd -> String -> IO [(Int, Word8)]
writeCustomChars lcd chars = do
  let ref = lcdCustom lcd
  ci <- readIORef ref
  let ci' = allocateCustomChars ci chars
      oldNew = zip3 [0..] (map fst $ snd ci) (map fst $ snd ci')
  forM oldNew $ \(i, old, new) -> do
    when (old /= new) $ do
      let (Just cd) = getCharData lcd new -- this should be safe
      lcdDefineChar (lcdCb lcd) i cd
    return (ord new, i)
